---
title:                "Swift: तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Kyun
Aaj ke digital yug mein, humein apne programming projects mein dates ko string mein convert karna bahut zaruri hai. Isse hum uss date ko easily readable aur displayable bana sakte hain. Is blog post mein hum aapko batayenge ki kis tarah se aap Swift mein dates ko string mein convert kar sakte hain.

# Kaise Karein
Iske liye, humein Swift programming language mein ek inbuilt function "dateFormatter()" ka istemal karna hoga. Is function ke through hum apne date ko string mein convert kar sakte hain.

```
// Input date
let date = Date()

// Create date formatter
let formatter = DateFormatter()

// Set date format
formatter.dateFormat = "dd.MM.yy"

// Convert date to string
let stringDate = formatter.string(from: date)

// Output string
print(stringDate)

```

Output:
12.03.21

Is coding example mein humne sabse pehle ek input date li hai. Fir, humne "formatter" naam ka ek DateFormatter object create kiya hai. Uske baad, humne date format ko "dd.MM.yy" set kiya hai. Isse humein output string mein "12.03.21" milega, jis date ko humne input kiya tha. Aap is format ko apne requirements ke according customize kar sakte hain.

# Deep Dive
DateFormatter ke alawa bhi kayi aur tariko se Swift mein dates ko string mein convert kiya ja sakta hai. Jaise ki, date ko "string interpolation" ka istemal karke bhi convert kiya ja sakta hai. Iske alawa, date ki properties jaise ki "month", "year", "day" ko bhi combine karke string mein convert kiya ja sakta hai.

Is tarah se, aap apne projects mein dates ko string mein convert karke unhe manipulate aur display kar sakte hain.

# Dekhiye Bhi
Is blog post mein humne aapko basic tarika bataya hai dates ko string mein convert karne ka. Agar aapko aur in-depth information chahiye toh neeche diye gaye links ko dekh sakte hain.

- [Official Apple documentation on DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Medium article on converting dates to strings in Swift](https://medium.com/learn-to-code-ios/swift-string-from-date-fa0ba758b79)
- [Tutorial on string interpolation in Swift](https://www.tutorialspoint.com/swift/swift_string_interpolation.htm)