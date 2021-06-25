use crate::turtle::Turtle;
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;

/**
 * TurtleRef describes a reference to a turtle that is owned by
 * the campus. These references may need to be passed around outside
 * Campus (in this case, the test harness needs access to them).
 * We could have specified the type precisely, but that would expose
 * implementation details of Campus to the clients.
 *
 * To avoid doing that, TurtleRef serves as an intermediary whenever
 * a reference to a Turtle needs to escape Campus.
 *
 * You will need to fill in TurtleRef with a field that reflects
 * your implementation. In order to manage lifetimes correctly,
 * TurtleRef does not provide access to a &Turtle; it instead
 * supplies a BorrowedTurtle, which implements Deref.
 * 
 * Another way of thinking about this is that TurtleRef provides 
 * a persistent reference, whereas a BorrowedTurtle is ephemeral,
 * generated temporarily while the Turtle is needed.
 *
 * An example use:
 *
 * let turtle_ref: TurtleRef = campus.get_turtle(...);
 * let turtle_name: &String = turtle_ref.borrowed_turtle().name();
 *
 * This code is here because the code below it relies on it. However, we suggest
 * implementing this AFTER you have figured out how to represent Campus, not before.
 */
pub struct TurtleRef {
    // Figure out what type r should have.
    // r: ???
    r: Rc<RefCell<Turtle>>
}

impl TurtleRef {
    pub fn borrowed_turtle(&self) -> BorrowedTurtle {
        BorrowedTurtle {
            r: self.r.borrow_mut()
        }
    }

    // We suggest implementing a 'new' function.
    // It's up to you to figure out what it should take as an argument.
    // pub fn new(r: ???) -> TurtleRef {
    //     TurtleRef{r}
    // }
}

// Hint: BorrowedTurtle might need a lifetime parameter.
pub struct BorrowedTurtle <'a> {
    // Figure out what type r should have.
    // r:
    r: RefMut<'a, Turtle>

}

// Hint: update this signature when you figure out the lifetime for BorrowedTurtle.
impl<'a> std::ops::Deref for BorrowedTurtle<'_> {
    type Target = Turtle;

    fn deref(&self) -> &Self::Target {
        return &*self.r;
    }
}

// Hint: update this signature when you figure out the lifetime for BorrowedTurtle.
impl<'a> std::ops::DerefMut for BorrowedTurtle<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        return & mut *self.r;
    }
}

pub struct Campus {
    // Fill in some fields here.
    turtles: Vec<TurtleRef>,
    cache: HashMap<String, Rc<RefCell<Vec<TurtleRef>>>>
}

impl Campus {
    pub fn new() -> Campus {
        return Campus {
            turtles: Vec::new(),
            cache: HashMap::new()
        };
    }

    /**
     * Returns the number of turtles present.
     */
    pub fn size(&self) -> usize {
        return self.turtles.len();
    }

    /**
     * This moves 'turtle', taking ownership. Do NOT implement Copy for Turtle.
     *
     * After add_turtle returns, the Campus should hold the turtle in its collection of turtles. The new turtle should be at the END of the collection.
     */
    pub fn add_turtle(&mut self, turtle: Turtle) {
        let new_turtle_ref = TurtleRef {
            r: Rc::new(RefCell::new(turtle))
        };
        self.turtles.push(TurtleRef {
            r: new_turtle_ref.r.clone()
        });


        match self.cache.get(new_turtle_ref.r.borrow().name()) {
            None => {
                let new_turtle_vec = Rc::new(RefCell::new(Vec::new()));
                new_turtle_vec.borrow_mut().push(TurtleRef {
                    r: new_turtle_ref.r.clone()
                });
                self.cache.insert(new_turtle_ref.r.borrow().name().clone(), new_turtle_vec);
            },
            Some (turtle_vec) => {
                turtle_vec.borrow_mut().push(TurtleRef {
                    r: new_turtle_ref.r.clone()
                });}
        };
    }

    /**
     * Gets a reference to a turtle at an index. Panics if index is out of bounds.
     */
    pub fn get_turtle(&self, index: usize) -> TurtleRef {
        return TurtleRef {
            r: self.turtles[index].r.clone()
        };
    }

    /**
     * Breeds the turtles at the given turtle indices, adding the new turtle to the end of the turtle vector.
     * If the indices are out of bounds, the method should panic.
     *
     * Should probably call a method you add to Turtle, which itself
     * uses the various 'cross' methods on the fields to initialize the new Turtle.
     * The new turtle should have a walking speed of 1 (babies go slowly).    
     * The new turtle should have its new favorite flavor selected randomly, 
     * and the new favorite color should be the result of crossing the 
     * parents' favorite colors.
     */
    pub fn breed_turtles(&mut self, t1_index: usize, t2_index: usize, child_name: String) {
        let new_turtle = self.turtles[t1_index].r.borrow_mut().breed(&mut self.turtles[t2_index].r.borrow_mut(), child_name.clone());
        let cloned_turtle = new_turtle.clone();
        match self.cache.get(&child_name) {
            None => {
                let new_turtle_vec = Rc::new(RefCell::new(Vec::new()));
                new_turtle_vec.borrow_mut().push(TurtleRef {
                    r: cloned_turtle
                });
                self.cache.insert(child_name.clone(), new_turtle_vec);
            },
            Some (turtle_vec) => {
                turtle_vec.borrow_mut().push(TurtleRef {
                    r: cloned_turtle
                });}
        };
        self.turtles.push(TurtleRef{
            r: new_turtle
        });
    }

    /**
     * Returns None if the campus is empty. Otherwise, returns Some of a reference to the turtle with the fastest walking speed.
     */
    pub fn fastest_walker(&self) -> Option<TurtleRef> {
        if self.turtles.len() == 0 {
            return None;
        }
        let mut fastest_turtle_index = 0;
        for x in 1..self.turtles.len() {
            if self.turtles[x].r.borrow().walking_speed() > self.turtles[fastest_turtle_index].r.borrow().walking_speed() {
                fastest_turtle_index = x;
            }
        }
        return Some(TurtleRef {
            r: self.turtles[fastest_turtle_index].r.clone()
        });
    }

    /**
     * Implement this for "Finding Testudo".
     * This interface will NOT work for "Fast Turtle Lookup".
     */
    pub fn turtles_with_name(&self, name: &str) -> Vec<TurtleRef> {
        let mut matched_turtles = Vec::new();
        for turtle in self.turtles.iter() {
            if turtle.r.borrow().name() == name {
                matched_turtles.push(TurtleRef {
                    r: turtle.r.clone()
                });
            }
        }
        return matched_turtles;
    }   

    pub fn turtles_with_name_cached(&self, name: &str) -> Rc<Vec<TurtleRef>> {
        let mut turtle_name_vec = Vec::new();
        match self.cache.get(name) {
            None => return Rc::new(turtle_name_vec),
            Some (turtle_vec_cell) => {
                for turtle in turtle_vec_cell.borrow_mut().iter() {
                    turtle_name_vec.push(TurtleRef {
                        r: turtle.r.clone()
                    });
                }
            }
        }
        
        return Rc::new(turtle_name_vec);
    }
}
