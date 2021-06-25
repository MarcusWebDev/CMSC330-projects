def fib(n)
    fibArray = []
    if n == 0
        return fibArray
    end

    i = 0
    while i < n
        fibArray[i] = calculateFib(i)
        i = i + 1
    end

    return fibArray
end

def calculateFib(n)
    if (n < 2)
        return n
    end

    return calculateFib(n-1) + calculateFib(n-2)
end

def ispalindrome(n)
    nstring = n.to_s
    first = 0
    last = nstring.length - 1
    while first <= last
        if nstring[first] != nstring[last]
            return false
        end

        first = first + 1
        last = last - 1
    end

    return true
end

def nthmax(n, a)
    if n > a.length - 1 || n < 0
        return nil
    end

    sortedArray = a.sort
    return sortedArray[sortedArray.length - 1 - n]
end

def freq(s)
    if s == ""
        return ""
    end

    i = 0
    frequency = Hash.new(0)
    while i < s.length
        frequency[s[i]] = frequency[s[i]] + 1
        i = i + 1
    end

    maxIndex = frequency.values.find_index(frequency.values.max)

    return frequency.keys[maxIndex]
end

def zipHash(arr1, arr2)
    if arr1.length != arr2.length
        return nil
    end

    myHash = Hash.new
    i = 0
    while i < arr1.length
        myHash[arr1[i]] = arr2[i]
        i = i + 1
    end

    return myHash
end

def hashToArray(hash)
    keysArray = hash.keys
    valuesArray = hash.values
    hashArray = Array.new(keysArray.length) {Array.new(2)}
    i = 0
    
    while i < keysArray.length
        hashArray[i][0] = keysArray[i]
        hashArray[i][1] = valuesArray[i]
        i = i + 1
    end
    
    return hashArray
end
