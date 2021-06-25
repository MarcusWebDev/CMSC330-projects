use crate::turtle::Turtle;

// A short name for a longer type. Documentation can be found here:
// https://doc.rust-lang.org/reference/items/type-aliases.html.
pub type TurtleRef<'a> = &'a Turtle;

pub struct Campus {
    turtles: Vec<Turtle>
}

impl Campus {
    pub fn new() -> Campus {
        return Campus {
            turtles: Vec::new()
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
        self.turtles.push(turtle);
    }

    /**
     * Gets a reference to a turtle at an index. Panics if index is out of bounds.
     */
    pub fn get_turtle(&self, index: usize) -> TurtleRef {
        return &self.turtles[index];
    }

    /**
     * Returns a fresh iterator
     */
    pub fn turtles(&self) -> std::slice::Iter<Turtle> {
        return self.turtles.iter();
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
        let new_turtle = self.turtles[t1_index].breed(&self.turtles[t2_index], child_name);
        self.add_turtle(new_turtle);
    }

    /**
     * Returns None if the campus is empty. Otherwise, returns Some of a reference to the turtle with the fastest walking speed.
     */
    pub fn fastest_walker(&self) -> Option<&Turtle> {
        if self.turtles.len() == 0 {
            return None;
        }
        let mut fastest_turtle_index = 0;
        for x in 1..self.turtles.len() {
            if self.turtles[x].walking_speed() > self.turtles[fastest_turtle_index].walking_speed() {
                fastest_turtle_index = x;
            }
        }
        return Some(&self.turtles[fastest_turtle_index]);
    }

    /**
     * Implement this for "Finding Testudo".
     * This interface will NOT work for "Fast Turtle Lookup".
     */
    pub fn turtles_with_name(&self, name: &str) -> Vec<TurtleRef> {
        let mut matched_turtles = Vec::new();
        for turtle in self.turtles.iter() {
            if turtle.name() == name {
                matched_turtles.push(turtle);
            }
        }
        return matched_turtles;
    }
}
