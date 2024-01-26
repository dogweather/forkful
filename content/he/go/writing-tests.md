---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות (Tests) בתכנות זה עיצוב של תרחישים כדי לוודא שהקוד שלך עובד כשורה. פרוגרמרים עושים את זה כדי למנוע באגים, לשפר איכות ולהקל על תחזוקת הקוד בעתיד.

## איך לעשות:

```Go
package main

import (
    "testing"
    "reflect"
)

// Add מחברת שני מספרים
func Add(a, b int) int {
    return a + b
}

// TestAdd טסט לפונקציה Add
func TestAdd(t *testing.T) {
    got := Add(3, 4)
    want := 7
    
    if got != want {
        t.Errorf("התקבל %d, רוצים %d", got, want)
    }
}
```
תוצאת הדוגמא: אם הבדיקה תעבור, לא יהיו הדפסות. אם יש כישלון, תראה הודעת שגיאה.

## צלילה עמוקה

בדיקת קוד היא חלק ממתודולוגיות פיתוח מודרניות כמו TDD (Test-Driven Development). יש אלטרנטיבות לבדיקות יחידה כמו בדיקות אינטגרציה או בקרה ידנית, אבל הן לא מחליפות אותן. ב-Golang, המחלקה "testing" מספקת את הפריימוורק לכתיבת טסטים.

## ראה גם:

- מידע נוסף על כתיבת טסטים ב-Golang בתיעוד הרשמי: [https://golang.org/pkg/testing/](https://golang.org/pkg/testing/)
- המדריך ל-TDD ב-Golang מבית היוצר של "Learn Go with Tests": [https://github.com/quii/learn-go-with-tests](https://github.com/quii/learn-go-with-tests)
- הפלאגין של GoConvey לבדיקות מתקדמות ב-Golang: [http://goconvey.co/](http://goconvey.co/)
