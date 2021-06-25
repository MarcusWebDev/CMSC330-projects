use crate::cookbook::{Cookbook, Recipe};
use crate::genetics::*;
use crate::magic::{TurtlePower, World};

#[derive(Debug)]
pub struct Turtle {
    name: String,
    walking_speed: u32,
    favorite_color: Color,
    favorite_flavor: Flavor,
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
            favorite_flavor: favorite_flavor
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

    pub fn breed(&self, partner: &Turtle, child_name: String) -> Turtle {
        return Turtle::new(child_name, 1, Color::cross(self.favorite_color(), partner.favorite_color()), Flavor::random_flavor());
    }

    /**
     * Returns Some of any recipe from the given cookbook that matches the turtle's flavor
     * preferences, or None if no such recipe exists.
     *
     * IMPORTANT: you will need to add lifetime parameters to this function. It is
     * up to you to figure out which ones and where. Do not make any other changes
     * to the signature.
     */
    pub fn choose_recipe<'a>(&self, cookbook: & 'a Cookbook) ->  Option<& 'a Recipe> {
        for recipe in cookbook.recipes() {
            if recipe.flavor() == self.favorite_flavor() {
                return Some(&recipe);
            }
        }
        return None;
    }
}
