---
title:                "Swift: दो तारीखों की तुलना"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyun

Kya aap kabhi socha hai ki aap sabhi tarikhon ke beech mein dhundh rahe hain? Aapko ek specific tarikh ke saath kaam karna hai aur aap chahtey hain ki kya woh tarikh kisi different tarikh se pehle ya baad mein hai? Aapko pareshan hone ki zaroorat nahin hai! Swift programming ke madhyam se, aap aasani se do tarikhon ko compare kar sakte hain.

## Kaise Karein

Tarikhon ko compare karne ke liye, hum "Date" data type ka istemaal karenge. Yeh Swift mein built-in data type hai aur humein exact tareeke se tarikhon ka kaam karna allow karta hai. Sabse pehle, hum do Date variables create karenge, jise hum "date1" aur "date2" naam se refer karenge.

```
Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 86400)
```

Yahan, humne "let" keyword ka istemaal kiya hai, kyunki humein yeh values change nahin karni hain. Ab humein "date1" aur "date2" ko compare karne ke liye, hum ek if-else statement ka istemaal karenge. Agar "date1" "date2" se pehle hai, toh hum print statement mein "date1 is before date2" print karenge, warna "date1 is after date2" print karenge.

```
Swift
if date1 < date2 {
    print("date1 is before date2")
} else {
    print("date1 is after date2")
}

// Output: date1 is before date2
```

Hamein apne code mein aur functions aur methods add kar sakte hain, jaise ki date formats change karna, specific tareekh ya waqt compare karna, lekin basic concept yahi hai.

## Gehraai mein jaana

Tarikhon ko compare karne ke liye, humare paas kuch important concepts hote hain jaise ki date formats, date components, aur time zones. Hum apne code mein in concepts ko implement kar sakte hain aur date comparison mein aur accuracy la sakte hain.

See Also:
- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Tutorial: Working with Dates in Swift](https://www.raywenderlich.com/160322/swift-tutorial-working-dates)
- [Hacking with Swift: Easy Date Calculations](https://www.hackingwithswift.com/example-code/dates/easy-date-calculations-using-swift)