---
title:    "Swift: מחיקת תווים התואמים תבנית"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה
למה למשתמש למחוק תווים שמתאימים לתבנית? טיפ קצר על המוטיבציה לביצוע פעולה זו.

## איך לעשות זאת
למחוק תווים מתאימים לתבנית ניתן להשתמש בפונקציות כגון `removeAll(where:)` או `filter()`. כאן תוכלו למצוא דוגמאות של קוד בשפת Swift עם תוצאות דוגמא לבירור.

```Swift
// דוגמאות למחיקת תווים מתאימים לתבנית לפי אורך התווים
var words = ["apple", "banana", "orange", "watermelon", "kiwi"]
words.removeAll { $0.count < 6 }
// תוצאה: ["banana", "watermelon"]

var numbers = [1, 2, 3, 4, 5, 6]
let evenNumbers = numbers.filter { $0 % 2 == 0 }
// תוצאה: [2, 4, 6]
```

## לצוף עמוק
מחיקת תווים מתאימים לתבנית היא טכניקה שימושית לעיבוד מידע באופן יעיל ומהיר. ניתן להיעזר בתכנות פונקציות ופעולות חדשות כדי להתאים את המחיקה לצרכי המשתמש. כמו כן, ניתן להשתמש בפונקציות נוספות כדי לשפר את הביצועים ולהגביר את היעילות של הקוד.

## ראו גם
- [מסמך רשמי של פונקציות בשפת Swift](https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html)
- [דוגמאות מאתר Stack Overflow עם פתרון למחיקת תווים על פי תבנית בSwift](https://stackoverflow.com/questions/26580123/whats-the-best-way-to-remove-all-of-character-from-a-string/40804963#40804963)