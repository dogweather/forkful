---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:51.188015-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות זה פשוט לקחת טקסט ולהפוך את כל האותיות לגרסת האלפאבית הקטנה שלהן. מתכנתים עושים את זה כדי לנרמל קלט, לשפר השוואה בין מחרוזות ולהפטר מבעיות רגישות לרישיות.

## איך לעשות:
בגו, אפשר להמיר מחרוזת לאותיות קטנות באמצעות הפונקציה `strings.ToLower()`. דוגמת קוד:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "שלום, עולם!"
	lowercase := strings.ToLower(original)
	fmt.Println(lowercase)
}
```

פלט לדוגמה:
```
שלום, עולם!
```

## צלילה עמוקה:
בעבר, תכנות לא תמיד טיפל ברגישות לרישיות באופן אחיד. למעשה, יכולות חיפוש והשוואה היו לעיתים רגישות להבדלים בין 'A' ל-'a'. בגו, המרה לאותיות קטנות היא פעולה פשוטה ויעילה, אבל חשוב לדעת שהיא עובדת עם קוד UTF-8, שהוא הסטנדרט לתווים ברוב המערכות העכשוויות. אם אתם מעוניינים בהמרה מותאמת תרבותית (למשל, המרה של האות 'İ' הטורקית ל-'i' קטנה ולא ל-'ı' קטנה), ייתכן שתצטרכו להשתמש בספריות אחרות או להתמודד עם מקרי קצה באופן פרטני.

## ראה גם:
- מסמכי גו על חבילת strings: https://golang.org/pkg/strings/
- מאמר על המרת מחרוזת לאותיות קטנות והעלאות: https://www.yourbasic.org/golang/lowercase-uppercase/
- ויקיפדיה על UTF-8: https://he.wikipedia.org/wiki/UTF-8
