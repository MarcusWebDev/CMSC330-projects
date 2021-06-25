class GameBoard
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @board = Array.new(max_row){Array.new(max_column, "-, -")}
        @ships = []
        @successful_attacks = 0
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        size = ship.size - 1

        case ship.orientation
        when "Right"
            if ship.start_position.row > @max_row || ship.start_position.row < 1 || ship.start_position.column + size > @max_column || ship.start_position.column < 1
                return false
            end

            for i in ship.start_position.column - 1..ship.start_position.column - 1 + size
                if @board[ship.start_position.row - 1][i] =~ /^B/
                    return false
                end
            end

            return put_ship_on_board(ship)
        when "Left"
            if ship.start_position.row > @max_row || ship.start_position.row < 1 || ship.start_position.column > @max_column || ship.start_position.column - size < 1
                return false
            end

            for i in (ship.start_position.column - 1).downto(ship.start_position.column - 1 - size)
                if @board[ship.start_position.row - 1][i] =~ /^B/
                    return false
                end
            end

            return put_ship_on_board(ship)
        when "Up"
            if ship.start_position.row > @max_row || ship.start_position.row - size < 1 || ship.start_position.column > @max_column || ship.start_position.column < 1
                return false
            end

            for i in (ship.start_position.row - 1).downto(ship.start_position.row - 1 - size)
                if @board[i][ship.start_position.column - 1] =~ /^B/
                    return false
                end
            end

            return put_ship_on_board(ship)
        when "Down"
            if ship.start_position.row  + size > @max_row || ship.start_position.row < 1 || ship.start_position.column  > @max_column || ship.start_position.column < 1
                return false
            end

            for i in ship.start_position.row - 1..ship.start_position.row - 1 + size
                puts i
                if @board[i][ship.start_position.column - 1] =~ /^B/
                    return false
                end
            end

            return put_ship_on_board(ship)
        else
            puts "Invalid ship orientation"
            return false
        end
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position
        if position.row > @max_row || position.row < 1 || position.column > @max_column || position.column < 1
            return nil
        end

        # update your grid
        if @board[position.row - 1][position.column - 1] =~ /^B.*[^A]$/
            @board[position.row - 1][position.column - 1] = "B, A"
            @successful_attacks = @successful_attacks + 1
        else
            @board[position.row - 1][position.column - 1] = "-, A"
        end

        # return whether the attack was successful or not
        if @board[position.row - 1][position.column - 1] =~ /^B/
            return true
        else
            return false
        end
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @successful_attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        for i in 0..@max_row - 1
            for j in 0..@max_column - 1
                if @board[i][j] =~ /^B.*[^A]$/
                    return false
                end
            end
        end

        return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        for i in 0..@max_row - 1
            for j in 0..@max_column - 1
                print "| " + @board[i][j] + " "
            end

            puts "|"
        end
    end

    def put_ship_on_board(ship)
        size = ship.size - 1

        case ship.orientation
        when "Right"
            for i in ship.start_position.column - 1..ship.start_position.column - 1 + size
                @board[ship.start_position.row - 1][i] = "B, -"
            end
        when "Left"
            for i in (ship.start_position.column - 1).downto(ship.start_position.column - 1 - size)
                @board[ship.start_position.row - 1][i] = "B, -"
            end
        when "Up"
            for i in (ship.start_position.row - 1).downto(ship.start_position.row - 1 - size)
                @board[i][ship.start_position.column - 1] = "B, -"
            end
        when "Down"
            for i in ship.start_position.row - 1..ship.start_position.row - 1 + size
                @board[i][ship.start_position.column - 1] = "B, -"
            end
            
        else
            puts "Cannot add to board"
            return false
        end

        puts "-------------------------"
        self.to_s
        return true
    end
end
