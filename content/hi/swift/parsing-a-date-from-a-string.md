---
title:                "शब्द से तारीख निर्धारित करना"
html_title:           "Swift: शब्द से तारीख निर्धारित करना"
simple_title:         "शब्द से तारीख निर्धारित करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Kya aur Kyon?
Parsing a date from a string ek common programming task hai jahan programmer ek string ko ek date mein convert karna chahte hain. Iska reason yeh hai ki date ko string format mein store karne se uska manipulation aur comparison difficulty hota hai. Isliye, programmers date ko string se parsing karke date objects mein store karte hain taaki unka code more readable aur efficient ho. 

# Kaise Karein:
Isko Swift mein parse karna bahut hi aasaan hai. Hum iske liye `DateFormatter` ka use karenge. Isko use karne ka basic syntax yeh hai: 
```
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date = dateFormatter.date(from: "2021-01-01")
print(date)  // 2021-01-01 00:00:00 +0000 
```
Humne pehle `DateFormatter` object banaya aur usko `dateFormat` property ke through humne specified format `yyyy-MM-dd` diya. Uske baad hum `date(from:)` method ko use karke string ko date object mein convert kiya. Output mein hum dekh sakte hain ki string successfully date object mein convert ho gaya hai. 

# Deep Dive:
Pehle, date parsing further complex bhi ho sakta hai jaise ki different time zones aur locale ko consider karna. Iske alawa, Swift ke alawa bhi bohot saare alternatives hain jaise ki `NSDateFormatter` aur `ISO8601DateFormatter` jo ki Objective-C aur ISO-8601 format ko respectively support karte hain. Date parsing ke liye more advanced techniques bhi hain jaise ki `Regular Expressions` aur `Natural Language Processing`. Inka use alag alag scenarios mein depend karta hai. 

# See Also:
1. [Date Parsing in Swift Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
2. [NSDateFormatter Documentation](https://developer.apple.com/documentation/foundation/nsdateformatter)
3. [ISO8601DateFormatter Documentation](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
4. [Regular Expressions Tutorial in Swift](https://www.raywenderlich.com/regex-swift)
5. [Natural Language Processing in Swift Tutorial](https://www.raywenderlich.com/natural-language-processing-tutorial-for-ios-getting-started)