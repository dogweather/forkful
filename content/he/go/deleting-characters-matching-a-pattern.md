---
title:                "מחיקת תווים המתאימים לתבנית"
html_title:           "Go: מחיקת תווים המתאימים לתבנית"
simple_title:         "מחיקת תווים המתאימים לתבנית"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# מה ולמה?

מחיקת תווים המתאימים לתבנית היא פעולת מחיקה שבה משנים את הטקסט על פי תבנית מסוימת. תהליך זה נמצא בשימוש נרחב בתכנות המחשב ובעיקר על מנת לנקות קלטים לא רצויים או למנוע שגיאות בתוכנית.

## איך לבצע:

כדי למחוק תווים המתאימים לתבנית בשפת Go, ניתן להשתמש בפקודת `strings.ReplaceAll()` ולהעביר את הטקסט המקורי, את התבנית לחיפוש ואת המחרוזת הריקה כתוצאה. ניתן גם להשתמש בפקודת `regex.ReplaceAllString()` ולהכניס את הטקסט המקורי, את התבנית לחיפוש ואת המחרוזת הריקה כתוצאה.

במקרה שהטקסט מכיל מספר מופעים של התבנית המבוקשת, יש להשתמש בפקודת `regex.ReplaceAllString()` כדי למחוק את כולם בפעם אחת.

## מעמקים:

מחיקת תווים המתאימים לתבנית היא פעולת טיפול במחרוזות נפתחת בניידות של תכנות אחרות, כמו Perl ו- AWK. בשפת Go, ניתן להשתמש גם בפקודות נוספות כגון `strings.Replace()` ו- `strings.Trim()` לטיפול במחרוזות.

בנוסף, ישנן אפשרויות נוספות למחיקת תווים בשפת Go, כגון פקודת `unicode.ReplaceAll()` שמשמשת לטיפול בפונטים ותווים מיוחדים. כמו כן, קיימות גם פקודות להחלפת תווים כגון `strings.Replace()` ו- `strings.ReplaceAll()`.

## ראה גם:

- [פקודת `strings.ReplaceAll()` מתוך התיעוד הרשמי של Go](https://golang.org/pkg/strings/#ReplaceAll)
- [כיצד לבצע פעולת מחיקה של תווים בשפת Go](https://www.sohamkamani.com/golang/string-operations/delete-characters-from-string/)