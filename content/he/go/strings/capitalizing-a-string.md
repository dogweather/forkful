---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:24.844501-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Go, \u05D4\
  \u05D7\u05D1\u05D9\u05DC\u05D4 `strings` \u05D0\u05D9\u05E0\u05D4 \u05DE\u05E1\u05E4\
  \u05E7\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05D9\u05E9\u05D9\u05E8\
  \u05D4 \u05DC\u05D4\u05E4\u05D9\u05DB\u05EA \u05D4\u05D0\u05D5\u05EA \u05D4\u05E8\
  \u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D1\u05DC\u05D1\u05D3\
  . \u05DC\u05DB\u05DF, \u05D0\u05E0\u05D5 \u05DE\u05E9\u05DC\u05D1\u05D9\u05DD \u05D0\
  \u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `strings.ToUpper()`,\u2026"
lastmod: '2024-03-13T22:44:38.458552-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Go, \u05D4\u05D7\u05D1\u05D9\u05DC\u05D4 `strings` \u05D0\u05D9\u05E0\
  \u05D4 \u05DE\u05E1\u05E4\u05E7\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05D4\u05E4\u05D9\u05DB\u05EA \u05D4\u05D0\
  \u05D5\u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  \ \u05D1\u05DC\u05D1\u05D3."
title: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA"
weight: 2
---

## איך לעשות:
ב-Go, החבילה `strings` אינה מספקת פונקציה ישירה להפיכת האות הראשונה של מחרוזת לאות גדולה בלבד. לכן, אנו משלבים את הפונקציה `strings.ToUpper()`, שהופכת מחרוזת לאותיות גדולות, עם חיתוך כדי להשיג את מטרתנו. כך תעשו זאת:

```go
package main

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

func CapitalizeFirst(str string) string {
    if str == "" {
        return ""
    }
    // בדוק אם התו הראשון כבר באות גדולה.
    if utf8.ValidString(str) && unicode.IsUpper([]rune(str)[0]) {
        return str
    }
    
    // המר את התו הראשון לאות גדולה
    r, size := utf8.DecodeRuneInString(str)
    return string(unicode.ToUpper(r)) + str[size:]
}

func main() {
    example := "hello, World!"
    fmt.Println(CapitalizeFirst(example)) // פלט: "Hello, World!"
}
```

הפונקציה הזו בודקת אם המחרוזת ריקה או אם התו הראשון כבר באות גדולה. היא משתמשת בחבילת `unicode/utf8` כדי לטפל נכונה בתווים של יוניקוד, ומבטיחה כי הפונקציה שלנו תעבוד עם מגוון רחב של קלטים מעבר ל-ASCII הבסיסי.

## הסבר נוסף
הצורך להפוך מחרוזות לאות ראשונה גדולה ב-Go ללא פונקציה מובנית יכול להיראות כמו מגבלה, במיוחד למתכנתים הבאים משפות בהן פונקציות לטיפול במחרוזות הן יותר מקיפות. אילוץ זה מעודד הבנה של טיפול במחרוזות וחשיבות היוניקוד בפיתוח תוכנה מודרני.

בהיסטוריה, שפות תכנות התפתחו בהתמודדות שלהן עם מחרוזות, כאשר שפות מוקדמות לעיתים קרובות התעלמו מאינטרנציונליזציה. גישת Go, למרות שדורשת קצת יותר קוד עבור משימות שנראות פשוטות, מבטיחה שמפתחים יהיו מודעים למשתמשים גלובליים מההתחלה.

ישנן ספריות מחוץ לספרייה הסטנדרטית, כמו `golang.org/x/text`, המציעות יכולות מתקדמות יותר של טיפול בטקסט. עם זאת, השימוש באלה צריך להישקל ביחס להוספת תלות חיצוניות לפרויקט שלך. עבור יישומים רבים, חבילות ה-`strings` ו-`unicode/utf8` של הספרייה הסטנדרטית מספקות כלים מספיקים לטיפול במחרוזות באופן יעיל ואפקטיבי, כפי שנראה בדוגמא שלנו. זה שומר על תוכניות Go רזות וקלות לתחזוקה, מהדהד את פילוסופיית השפה של פשטות ובהירות.
