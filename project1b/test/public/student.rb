require "minitest/autorun"
require_relative "../../src/controllers/input_controller.rb"
require_relative "../../src/controllers/game_controller.rb"
require_relative "../../src/models/game_board.rb"
require_relative "../../src/models/position.rb"
require_relative "../../src/models/ship.rb"

SHIPS_P1 = "#{__dir__}/student_ships.txt"
SHIPS_P2 = "#{__dir__}/student2_ships.txt"

ATTACK_P1 = "#{__dir__}/student_attacks.txt"
ATTACK_P2 = "#{__dir__}/student2_attacks.txt"

class PublicTests < MiniTest::Test
    def setup



    end

    def test_my_stuff
        test_board = GameBoard.new 10, 10
        assert(read_ships_file(SHIPS_P1))
        refute(read_ships_file(SHIPS_P2))



    end
end