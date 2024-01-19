---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה זה ולמה? [What & Why?]

הסרת תווים התואמים לדפוס היא פעולה שבה אנו מסירים תווים מסוימים ממחרוזת. מדובר באסטרטגיה שמפתחים משתמשים בה כדי לנקות נתונים ולוודא שהקוד שלהם מריץ בצורה תקנית ועקבית.

## איך לעשות את זה? [How to]

Go הוא שפת תכנות מקצועית, חזקה, המציעה מערכות עמידות שימושיות. אם אתה רוצה למחוק אותיות מתווים שמתאימות לצורה מסוימת, תוכל להשתמש בפונקציה `strings.ReplaceAll()`. 

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, Programmer!"
	newStr := strings.ReplaceAll(str, "o", "")
	fmt.Println(newStr) 
}
```
הפלט יהיה:
```
Hell, Prgrammer!
```

## ביקורת עמוקה [Deep Dive]

העקרון של מחיקת תווים מתוך מחרוזת אינו חדש. שפות תכנות אחרות, כמו Python או Java, מציעות גם הן יכולת זו. יתרון המתכנת של Go במקרה זה הוא היכולת לעשות זאת בצורה אפקטיבית יותר מבחינה של משאבי מערכת. 

ישנם גם שיטות חלופות לביצוע משימה זו, אחת מהן היא שימוש ב-Regexp. אפשר לכתוב דפוס משלך ולהסיר תווים לפי התאמה לדפוס עם `Regexp.ReplaceAllString()`. 

במעמד הקומפילציה, הפונקציה `strings.ReplaceAll()` מחזירה מחרוזת חדשה שבה כל מופע של המחרוזת המיוחדת הוחלף במחרוזת הנתונה.

## ראה גם [See Also]

בדקו אותם את מסמכי Go עבור פונקציות נוספות של מחרוזות: https://golang.org/pkg/strings/

לקבלת מידע נוסף על ביטויים רגילים ב-Go, נסה: https://golang.org/pkg/regexp/