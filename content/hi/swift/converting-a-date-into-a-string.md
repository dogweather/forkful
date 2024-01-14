---
title:                "Swift: एक तारीख को स्ट्रिंग में रूपांतरित करना"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyun

Aksar hume apne programming projects mein dates ko string mein convert karna padta hai. Iska sabse bada karan hai ki dates ko string mein convert karna humare code ko readable aur understandable banata hai. Saath hi, dates ki formatting ko bhi hum aise set kar sakte hai jaisa hum chahte hai.

## Kaise

Swift mein, hum Date() data type se dates ko represent karte hai. Lekin, ye data type humare liye dates ko string mein convert nahi karta hai. Iske liye hume DateFormatter class ka istemaal karna padta hai. Is class se hume ek formatter create karna padega, jisme hum dates ki formatting ko set kar sakte hai. Fir hum uss formatter ko use karke Date ko string mein convert kar sakte hai. Yeh kaam hum `string(from: Date)` method se kar sakte hai. Neeche diye gaye code blocks mein iska example diya gaya hai:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy" // Humne date ki formatting set ki hai
let today = Date()
let dateString = dateFormatter.string(from: today)
print(dateString) // Output: 05/07/2020
```

Is code mein humne `dateFormatter` create kiya aur usme `dateFormat` set kiya. Iske baad hum `string(from: Date)` method ka use karke `today` ko string mein convert kiya aur fir ussi string ko print kiya.

## Badi Gehrayi

Dates ko string mein convert karna thoda sa complex ho sakta hai. Humne example mein ek simple formatting use kiya lekin humare paas aur bhi options hai formatting ki. Hum apne date format mein `yyyy` likh kar usme 4 ya 2 digits ko add kar sakte hai, jisse year ki length ko hum control kar sake. Saath hi, hum `MMM` ya `MMMM` ka use karke bhi month ko alag-alag tarikon se represent kar sakte hai.

Saath hi, dates ki formatting ke alawa hum isme time zone aur localizations bhi set kar sakte hai. Isse hum apne output ko apne hisaab se customize kar sakte hai. Iske liye hume DateFormatter ke `timeZone` aur `locale` properties ka use karna padega.

## Dekhein Bhi

["Working with Dates in Swift"](https://www.hackingwithswift.com/read/11/2/working-with-dates-in-swift) <br>
["Formatting Dates in Swift using DateFormatter"](https://medium.com/@javedmultani16/formatting-dates-in-swift-using-dateformatter-55db75c593e7)