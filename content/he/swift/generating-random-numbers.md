---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:50:29.533774-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
גנרטור מספרים אקראיים זה כלי שמייצר קבוצה של מספרים שלא ניתן לחזות אותם מראש. תוכניתנים משתמשים בזה למגוון צרכים: ממשחקים ועד בדיקות אבטחה.

## איך לעשות:
קוד ב-Swift ליצירת מספרים אקראיים:

```Swift
import Foundation

// מספר אקראי שלם בין 1 ל-100
let randomNumber = Int.random(in: 1...100)
print(randomNumber)

// מספר אקראי שבר עשרוני בין 0 ל-1
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)
```
תוצאות דוגמה:
```
42
0.84375
```
כל פעם שתריץ את הקוד, תקבל מספרים שונים.

## צלילה עמוקה:
בהיסטוריה, ייצור מספרים אקראיים היה אתגר. אלגוריתמים מוקדמים כמו LCG (Linear Congruential Generator) נוצרו אבל לא היו מספיק טובים לכל השימושים. ב-Swift, מנוע ההגרלה אינו אקראי לחלוטין (כמו במרבית השפות). הוא "פסאודו-אקראי", מספק סדרה של מספרים שנראים אקראיים.

Swift משתמשת במנוע `arc4random_uniform()` עבור מספרים אקראיים, שהוא חלק ממערכת ההפעלה אפל ומותאם לאבטחה ולביצועים. יש גם את `random(in:)` שמטפל בגבולות בקלות יותר.

## ראה גם:
- [Apple Documentation on Random Number Generator](https://developer.apple.com/documentation/swift/randomnumbergenerator)
- [Wikipedia article on Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
