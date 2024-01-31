---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:46:18.843729-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא התהליך שבו ניתן לקחת חלק ממחרוזת גדולה יותר. תכנותים עושים זאת כדי לעבד רק את המידע הרלוונטי למטרות שלהם.

## איך לעשות:
Go מספקת פונקציות נוחות לחילוץ תת-מחרוזות. להלן דוגמאות שימוש ב`slice notation`:

```Go
package main

import (
	"fmt"
)

func main() {
	str := "שלום עולם"
	// חילוץ מחרוזת מתו השלישי עד החמישי (האינדקסים מתחילים מ-0)
	substr := str[6:12]
	fmt.Println(substr) // פלט: שלום

	// חילוץ התו הראשון עד התו החמישי
	prefix := str[:10]
	fmt.Println(prefix) // פלט: שלום

	// חילוץ מהתו החמישי עד סוף המחרוזת
	suffix := str[6:]
	fmt.Println(suffix) // פלט: עולם
}
```

שימו לב: עבור מחרוזות UTF-8 עם תווים בקידוד מרובת בתים, כמו בעברית, חשוב להשתמש בפונקציות שמטפלות בראנרים כדי למנוע חילוץ לא תקני שיכול לשבור את התווים.

## נסיון במעמקים
מבחינה היסטורית, חילוץ תת-מחרוזות היא פעולה בסיסית שנתמכת ברוב שפות התכנות. בGo, חשוב להיות מודעים לעובדה שגישה ישירה לאינדקסים במחרוזת UTF-8 עלולה להיות מוטעית בגלל התווים מרובי בתים. פונקציות כמו `string(runeSlice)` ו-`[]rune(str)` משמשות לטיפול נכון בתווים עבריים. ישנן ביבליות נוספות כמו "unicode/utf8" שמאפשרות עבודה מתקדמת עם תווים UTF-8.

## ראה גם
- [Go Slices: usage and internals](https://blog.golang.org/slices-intro)
- [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- [Package utf8](https://golang.org/pkg/unicode/utf8/)

שימו לב שלא כל המקורות זמינים בעברית, אך הם מספקים מידע איכותי ורלוונטי לנושא.
