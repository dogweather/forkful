---
title:                "מחברת מחרוזות"
html_title:           "Go: מחברת מחרוזות"
simple_title:         "מחברת מחרוזות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
פעולת חיבור מחרוזות היא פעולה נפוצה בתכנות, שמאפשרת למפרק ולחבר מחרוזות כדי ליצור מחרוזות חדשות. תהליך זה נחשב לשימושי כי ניתן להשתמש בו על מנת ליצור מחרוזות מאורגנות ומותאמות לצורך מסוים.

## איך לעשות זאת:
#### דוגמה 1:
```Go
str1 := "שלום,"
str2 := "עולם"
fmt.Println(str1 + str2) // Output: שלום, עולם
```

#### דוגמה 2:
```Go
str1 := "זה"
str2 := "יותר"
str3 := "ממשקים"
fmt.Printf("%s %s %s!", str1, str2, str3) // Output: זה יותר ממשקים!
```

## חפירה עמוקה:
פעולת חיבור מחרוזות נמצאת בשימוש ממזמן רב וניתן למצוא אותה במגוון שפות תכנות שונות. בשפת Go, ניתן להשתמש בפעולת חיבור מחרוזות באמצעות האופרטור `+` או להשתמש בפונקציות כמו `fmt. Print` כדי לשלב מחרוזות. פעולת חיבור מחרוזות אינה יעילה ביצירת מחרוזות גדולות, לכן כדאי להשתמש בפעולה רק ליצירת מחרוזות קטנות. בנוסף, ישנן שפות תכנות אחרות שמציעות אלטרנטיבות לפעולת חיבור מחרוזות, כגון שימוש בפעולת ההשמה (`concatenation`) או בפעולת התבניות (`templates`).

## ראה גם:
פעולת חיבור מחרוזות בגובייה: https://gobyexample.com/string-concatenation

פעולות אחרות נפוצות בשפת Go: https://golang.org/pkg/strings/

מדריך לשימוש בפעולת הכנסת מחרוזות בשפת Go: https://www.digitalocean.com/community/tutorials/how-to-use-the-concatenation-operator-and-strconv-in-go