require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    newBoard = GameBoard.new 10, 10
    numShips = 0
    read_file_lines(path) {|string| 
        lineArray = string.split
        if numShips >= 5
             return newBoard
        end

        lineArray[1] = lineArray[1].delete(",")
        lineArray[2] = lineArray[2].delete(",")

        if lineArray[0] =~ /^\(([0-9]+),([0-9]+)\)/ && lineArray[1] =~ /^(Up|Down|Right|Left)$/ && lineArray[2] =~ /^[1-5]$/
            lineArray[0] =~ /^\(([0-9]+),([0-9]+)\)/
            row = $1.to_i
            column = $2.to_i
            newPosition = Position.new(row, column)
            newShip = Ship.new(newPosition, lineArray[1], lineArray[2].to_i)
            if newBoard.add_ship(newShip)
                numShips = numShips + 1
            end
        end 
    }

    if numShips != 5 
        return nil
    end

    return newBoard
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    positionArray = Array.new()
    index = 0
    read_file_lines(path) {|string|
        if string =~ /^\(([0-9]+),([0-9]+)\)$/
            newPosition = Position.new($1.to_i, $2.to_i)
            positionArray[index] = newPosition
            index = index + 1
        end
    }
    return positionArray
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
