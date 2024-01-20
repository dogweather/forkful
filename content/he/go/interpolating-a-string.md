---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?: 
אינטרפולציה של מחרוזת היא תהליך של הכנסת משתנים או ביטויים למחרוזת. מתכנתים משתמשים במתודה זו כדי להפוך את החלקים הדינמיים של מחרוזת לקריאים ומסודרים יותר.

## איך לעשות: 
```Go
name := "Gopher"
fmt.Printf("שלום %s, איך אני יכול לעזור לך היום?", name)
```
פלט צפוי:
```
שלום Gopher, איך אני יכול לעזור לך היום?
```

## צלילה עמוקה: 
שפת Go מאפשרת את לעיבוד מחרוזת במספר דרכים, אם כי ברוב המקרים מספיק להשתמש ב- 'fmt.Printf' לביצוע המשימה. 

אפשרויות נוספות כוללות את 'fmt.Sprintf', שמחזירה מחרוזת במקום להדפיסה:

```Go
name := "Gopher"
str := fmt.Sprintf("שלום %s, איך אני יכול לעזור לך היום?", name)
fmt.Println(str)
```

מרבית המימושים משתמשים ב-Sprintf בתוך פונקציות 'log' או 'fatal' של Go ליצירת הודעות שגיאה מותאמות אישית.

## ראה גם:
[מסמך המימוש של מחרוזת ב-Go](https://golang.org/doc/)
[הדרכה מקוונת לאינטרפולציה של מחרוזת ב-Go](https://tour.golang.org/)
[מדריך לאינטרפולציה של מחרוזת ב-Go ב-Stack Overflow](https://stackoverflow.com/)