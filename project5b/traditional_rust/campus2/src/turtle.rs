use crate::cookbook::{Cookbook, Recipe};
use crate::genetics::*;
use crate::magic::{TurtlePower, World};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Turtle {
    name: String,
    walking_speed: u32,
    favorite_color: Color,
    favorite_flavor: Flavor,
    magical_item: Option<Rc<RefCell<Box<dyn TurtlePower>>>>,
    child_vec: Vec<Rc<RefCell<Turtle>>>
}

pub struct NoMagicalItemError;

impl Turtle {
    /**
     * Returns an appropriately-initialized Turtle.
     */
    pub fn new(name: String, walking_speed: u32, favorite_color: Color, favorite_flavor: Flavor) -> Turtle {
        let new_turtle = Turtle {
            name: name,
            walking_speed: walking_speed,
            favorite_color: favorite_color,
            favorite_flavor: favorite_flavor,
            magical_item: None,
            child_vec: Vec::new()
        };
        return new_turtle;
    }

    pub fn walking_speed(&self) -> u32 {
        return self.walking_speed;
    }

    pub fn favorite_flavor(&self) -> Flavor {
        return self.favorite_flavor;
    }

    pub fn name(&self) -> &String {
        return &self.name;
    }

    pub fn favorite_color(&self) -> &Color {
        return &self.favorite_color;
    }

    pub fn breed(&mut self, partner: &mut Turtle, child_name: String) -> Rc<RefCell<Turtle>> {
        let new_turtle = Turtle::new(child_name, 1, Color::cross(self.favorite_color(), partner.favorite_color()), Flavor::random_flavor());
        let new_turtle_ref = Rc::new(RefCell::new(new_turtle));
        self.child_vec.push(new_turtle_ref.clone());
        partner.child_vec.push(new_turtle_ref.clone());
        return new_turtle_ref;
    }

    /**
     * Returns Some of any recipe from the given cookbook that matches the turtle's flavor
     * preferences, or None if no such recipe exists.
     *
     * IMPORTANT: you will need to add lifetime parameters to this function. It is
     * up to you to figure out which ones and where. Do not make any other changes
     * to the signature.
     */
    pub fn choose_recipe<'a>(&self, cookbook: & 'a Cookbook) -> Option<& 'a Recipe> {
        for recipe in cookbook.recipes() {
            if recipe.flavor() == self.favorite_flavor() {
                return Some(&recipe);
            }
        }
        return None;
    }

    /**
     * This function should take ownership of the magical item and save it in a field of 'self' for future use.
     * If there is already a magical item, the new item should be saved (the old item can be discarded).
     */
    pub fn take_magical_item(&mut self, item: Box<dyn TurtlePower>) {
        self.magical_item = Some(Rc::new(RefCell::new(item)))
    }

    /**
     * If there is a saved magical item, this function should activate it and return Ok(()).
     * Otherwise, it should return the appropriate error.
     */
    pub fn activate_magical_item(&mut self, world: &mut World) -> Result<(), NoMagicalItemError> {
        match &self.magical_item {
            None => return Err(NoMagicalItemError),
            Some(item) => item.clone().borrow_mut().activate(world) 
        }
        return Ok(());
    }

    /**
     * Returns the number of children that this turtle has.
     */
    pub fn num_children(&self) -> usize {
        return self.child_vec.len()
    }

    /**
     * Gives every child walking lessons, which increases their walking speed by 1.
     * (You think walking is easy? Try doing it while carrying your house on your back!)
     */
    pub fn teach_children(&mut self) {
        for turtle in self.child_vec.iter() {
            turtle.borrow_mut().walking_speed += 1;
        }
    }
}
