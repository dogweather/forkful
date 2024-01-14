---
title:                "Swift: ממזג מחרוזות"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה
מחשבות כיצד ניתן ליצור מחרוזות סדורות ואותן הרבה פעמים יש צורך לחבר כמה מחרוזות כדי ליצור מחרוזת אחת ארוכה יותר. מאחר שמחרוזות משמשות כדי להציג מידע למשתמש או לסדרת פעולות, ייתכן שתרצו ללמוד כיצד לחבר מחרוזות בשפת סוויפט.

## איך לעשות
החיבור של שתי מחרוזות בשפת סוויפט יכול להתבצע בעזרת האופרטור "+" או באמצעות הפעולה "append". למשל:

```Swift 
var message = "שלום"
var name = "דני"
print(message + name) 
// פלט: שלוםדני

var answer = "42"
var explanation = "התשובה לשאלת החיים, היקר"
answer.append(explanation) 
print(answer)
// פלט: 42התשובה לשאלת החיים, היקר
```

כמו כן, ניתן לחבר מחרוזות אלפאנומריות כדי ליצור מחרוזות יותר מורכבות:

```Swift 
var welcome = "ברוכים הבאים ל"
var location = "האתר המדהים שלנו"
var website = welcome + location
print(website)
// פלט: ברוכים הבאים להאתר המדהים שלנו
```

## מעמק
התחברות של מחרוזות נחשבת לפעולה יסודית בשפת סוויפט וחשובה להבנת יישומים רבים. כאשר משתמשים בשפת סוויפט כדי ליצור אפליקציות, ניתן להשתמש בחיבור מחרוזות כדי לייצר תוויות או ליצור מחרוזות מורכבות שימושיות למשתמש.

## ראו גם
- [מדריך קצר על פעולות מחרוזות בשפת סוויפט](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)
- [הוספת מחרוזות בשפת סוויפט](https://www.ios-blog.co.il/swift-how-to-concatenate-strings/)