---
title:    "Swift: יצירת מספרים אקראיים"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# למה

כתיבת קוד ב-Swift עשיר ומעניין מצריך להשתמש בסוג של מספרים אקראיים. יצירת מספרים אקראיים יכולה להיות שימושית בהשפעת פעולת הקוד, עבודה עם מספרים רנדומליים או ליצירת מידע דמהי.

# איך להשתמש במספרים אקראיים בשפת Swift

```swift
//  יצירת מספרים אקראיים בתחום מסוים
let randomNumber = Int.random(in: 1...100)

//  יצירת מספרים אקראיים מתוך מערך מסוים
let numbers = [1, 2, 3, 4, 5]
let randomIndex = Int.random(in: 0..<numbers.count)
let randomElement = numbers[randomIndex]

// מספר אקראי בין 0 ל-1
let randomDouble = Double.random(in: 0...1)

//  יצירת מספרים אקראיים אקראיים מסוימים
let randomInt = Int.random(in: -100...100)
let randomDouble = Double.random(in: -50.5...50.5)
```

## למה זה עובד

פונקציות random של שפת Swift מבוססות על מספרי מגרעת, כאשר הם מייצרים תוצאה אקראית על סמך מספרים קודמים בזמן הביצוע.

## להעמיק

כאשר מנסים ליצור מספרים אקראיים מתוך טווחים גדולים יותר, כדוגמת Int64 או Double, יש לפעול אחרת עם פונקציות random לגבי הגבולות הנכונים. ניתן לקרוא עוד על הגבולות וכיצד לעבוד איתם כדי ליצור מספרים אקראיים מגוונים יותר.

## ראו גם

- [מדריך לפונקציות random של שפת Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID334)
- [מאמר על יצירת מספרים אקראיים ב-Swift](https://medium.com/better-programming/random-numbers-in-swift-9f7878c54772)
- [מדריך נוסף על מספרים אקראיים בשפת Swift](https://www.ioscreator.com/tutorials/swift-random-numbers)