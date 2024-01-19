---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיצוב תת-מחרוזות הינו המנוהל שבו מזיקים מחרוזת למחרוזות ממוזערות. מתכנתים משתמשים בכך למידע ביניים, לניתוח נתונים, ולטיפול בטקסט.

## איך לעשות:
נפתח עם דוגמה פשוטה, חתיכת קוד Swift שמחלצת תת-מחרוזת:
```Swift
let str = "Hello, Swift"
let indexStart = str.index(str.startIndex, offsetBy: 7)
let indexEnd = str.index(str.startIndex, offsetBy: 12)
let substring = str[indexStart..<indexEnd]
print(substring) //"Swift"
```
הפלט הוא "Swift". סרגלים למטה מגדירים טווח החיצוב. 

## צלילה עמוקה:
השיטה הזו סביב מערך כמו של Swift הושפעה מ- Objective-C. האלטרנטיבות הפשוטות היו לחתוך את המחרוזת או אפילו להשתמש בספליט. פירוט על מנגנון החיצוב מכיל את האינדקסים והטווחים בתוך המחרוזת.

## ראו גם:
עיין במקורות אלו למידע נוסף:
1. מדריך Apple למחרוזות Swift: https://developer.apple.com/documentation/swift/string
2. מידע נוסף על טווחים במחרוזות Swift: https://www.hackingwithswift.com/example-code/strings/how-to-split-a-string-into-an-array