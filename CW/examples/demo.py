def getDayName(day):
    if day == 1:
        return "Monday"
    elif day == 2:
        return "Tuesday"
    elif day == 3:
        return "Wednesday"
    elif day == 4:
        return "Thursday"
    elif day == 5:
        return "Friday"
    elif day == 6:
        return "Saturday"
    elif day == 7:
        return "Sunday"
    else:
        return "Invalid day"

def someOtherFunction(x):
    for i in range(5):
        if x > i:
            x -= 1
        else:
            x += 1
    return x