---
title:                "दो तारीखों की तुलना करना"
html_title:           "Swift: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyun

Aksar hume apni applications mein do tarikhon ko compare karna hota hai, jaise ki kisi event ki start date aur end date. Swift mein dates ko compare karna kaafi aasaan hai aur iske liye aapko sirf thodi si coding aani chahiye.

## Kaise Karein

```Swift
let formatter = DateFormatter()

formatter.dateStyle = .short

formatter.dateFormat = "dd/MM/yyyy"

let firstDate = formatter.date(from: "15/08/2021")

let secondDate = formatter.date(from: "25/08/2021")

if firstDate == secondDate {
    print("Dono dates barabar hai.")
} else if firstDate < secondDate {
    print("First date chota hai second date se.")
} else {
    print("First date bada hai second date se.")
}
```

**Output:** First date chota hai second date se.

Yahan humne `DateFormatter` ka use kiya hai, jo hume date ko string mein convert karne mein madad karta hai. Fir humne `date(from:)` method se string ko date mein convert kiya. Phir hum `==` aur `<` operators ka use karke dates ko compare kar rahe hai. Aap is code snippet ko apni application mein use karke results dekh sakte hai.

## Deep Dive

Dates ko compare karne ke liye aapko `Date` structure ka use karna hoga. Iske alawa `DateFormatter` bhi kaafi important hai, jis se hum dates ko string mein aur vice versa convert kar sakte hai. Swift mein hum `==`, `<`, `>` operators ka use karke dates ko compare kar sakte hai. Iske alawa `compare(_:, to:)` method bhi available hai jo dates ko compare karne mein madad karta hai.

## Dekhiye Bhi

Agar aapko Swift aur dates ke baare mein aur jaankari chahiye, toh aap neeche diye gaye links check kar sakte hai:

- [Dates in Swift](https://developer.apple.com/documentation/foundation/date)
- [DateFormatter in Swift](https://developer.apple.com/documentation/foundation/dateformatter)