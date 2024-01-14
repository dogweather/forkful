---
title:                "Swift: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

ברוב המקרים, נמלטים למחיקת תווים תואמים לעצמם כדי לטפל בתווים תואמים שאינם מתאימים לצרכים שלנו. ככל שהמספר של התווים המתאימים הוא גדול יותר, כך נוכל להימלט ממספר גדול של אלו שאינם מתאימים.

## איך לעשות זאת

למצוא ולמחוק תווים תואמים ניתן לעשות בקלות בשפת סוויפט. להלן כמה דוגמאות של קוד ופלט נבנו בתבנית " ```רבב סוויפט ...```":

```Swift
let words = ["abc", "bcd", "cde", "def"]

for word in words {
    if word.contains("c") {
        print("המילה \(word) מכילה את האות 'c'")
    }
}
```

פלט: המילה bcd מכילה את האות 'c'
המילה cde מכילה את האות 'c'

```Swift
let numbers = [1, 2, 3, 4, 5]

let filteredNumbers = numbers.filter { $0 % 2 == 0 }

print(filteredNumbers)
```

פלט: [2, 4]

## חקירה מעמיקה

מחיקת תווים תואמים הנמצאים במחרוזת ניתן לעשות באמצעות פקודת `filter` שבשפת סוויפט. פקודה זו מקבלת לפרמטר נוסף פונקציה שמציינת את התנאי למחיקת התווים המתאימים. לכן, ניתן להשתמש בפקודה זו למחיקת כל תווים שאינם מתאימים לצרכים שלנו.

## ראו גם

- [מדריך קצר על פקודת filter בסוויפט](https://www.hackingwithswift.com/example-code/language/whats-the-difference-between-map-filter-and-reduce)
- [מדריך מפורט על פקודת filter ותוספות שניתן לעשות עמה](https://www.swiftbysundell.com/basics/filter/)