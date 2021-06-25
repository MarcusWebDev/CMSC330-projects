class PhoneBook
    def initialize()
        @listedPhonebook = Hash.new()
        @unlistedPhonebook = Hash.new()

    end

    def add(name, number, is_listed)
        if (@listedPhonebook.has_key?(name) || @unlistedPhonebook.has_key?(name)) || !(number =~ /^[0-9]{3}-[0-9]{3}-[0-9]{4}$/) || (is_listed == true && @listedPhonebook.has_value?(number))
            return false
        end

        if is_listed == true
            @listedPhonebook[name] = number
            return true
        end
        if is_listed == false
            @unlistedPhonebook[name] = number
            return true
        end
    end

    def lookup(name)
        return @listedPhonebook[name]
    end

    def lookupByNum(number)
        numberIndex = @listedPhonebook.values.find_index(number)
        if numberIndex == nil
            return nil
        end

        return @listedPhonebook.keys[numberIndex]

    end

    def namesByAc(areacode)
        listedNames = @listedPhonebook.keys
        listedNumbers = @listedPhonebook.values
        unlistedNames = @unlistedPhonebook.keys
        unlistedNumbers = @unlistedPhonebook.values
        matchedNames = []

        i = 0
        currentIndex = 0
        while i < listedNumbers.length
            if (listedNumbers[i] =~ /^#{areacode}/)
                matchedNames[currentIndex] = listedNames[i]
                currentIndex = currentIndex + 1
                puts listedNames[i] + " listed"
            end
            i = i + 1
        end
        
        i = 0
        while i < unlistedNumbers.length
            if (unlistedNumbers[i] =~ /^#{areacode}/)
                matchedNames[currentIndex] = unlistedNames[i]
                currentIndex = currentIndex + 1
                puts unlistedNames[i] + " unlisted"
            end
            i = i + 1
        end

        return matchedNames
    end
end
