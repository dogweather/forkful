---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Swift: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Kyun
Soch rahe ho ki apna date ko string mein convert kyun karna chahiye? Yeh ek common problem hai jo developers face karte hai jab unhe apne apps mein date ko display karna hota hai. Date ko direct display karna thoda ajeeb lagta hai aur logon ko confusion ho sakta hai. Isiliye hum date ko string mein convert karte hai jisse hum usse apne hisab se format kar sake.

# Kaise Karein
Date ko string mein convert karne ke liye, hum DateFormatter class ka istemal karenge. Is class ke ek object ke through hum date ko string mein convert kar sakte hai. Chaliye ek example ke through dekhte hai:

```Swift
let date = Date() // current date
let formatter = DateFormatter()
formatter.dateFormat = "dd MMMM yyyy" // desired date format
let dateString = formatter.string(from: date)
print(dateString) // output: 14 January 2021
```

Is code mein humne ek current date li aur usse desired format mein string mein convert kiya. Date format ko apne hisab se change kar sakte hai jaise "yyyy/MM/dd" ya "MMMM dd, yyyy" aur bhi bahut sare formats hai jo aap apne project ke according choose kar sakte hai.

# Gehrai Mein Jaayein
DateFormatter class hume bahut sari functionality deta hai jaise hum time zone, locale, calendar, aur bhi bahut kuch specify kar sakte hai. Hum is class ke through ek specific date format nahi balki ek customized date format bhi use kar sakte hai. Agar aapke pass ek custom date format hai to aap use "dateFormat" property ke sath set kar sakte hai. Aur agar aapke pass ek date string hai to aap use "date(from:)" method ke sath Date object mein convert kar sakte hai.

See Also:
- https://developer.apple.com/documentation/foundation/dateformatter
- https://www.hackingwithswift.com/example-code/language/how-to-convert-dates-and-times-to-a-string-using-dateformatter