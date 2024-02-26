---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:04.775098-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05D3\u05E4\u05D5\u05E1 \u05DE\u05E1\u05D5\
  \u05D9\u05DD \u05D4\u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D4\u05E1\
  \u05D9\u05E8 \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\
  \u05DD \u05D0\u05D5 \u05E1\u05D3\u05E8\u05D5\u05EA \u05E9\u05DC \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05D1\u05D4\u05EA\
  \u05D1\u05E1\u05E1 \u05E2\u05DC \u05DB\u05DC\u05DC\u05D9\u05DD \u05E9\u05DE\u05D5\
  \u05D2\u05D3\u05E8\u05D9\u05DD \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D3\u05E4\u05D5\
  \u05E1 (\u05DC\u05E8\u05D5\u05D1 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D1\
  \u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\u2026"
lastmod: '2024-02-25T18:49:36.773980-07:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05D3\u05E4\u05D5\u05E1 \u05DE\u05E1\u05D5\u05D9\
  \u05DD \u05D4\u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D4\u05E1\u05D9\
  \u05E8 \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD\
  \ \u05D0\u05D5 \u05E1\u05D3\u05E8\u05D5\u05EA \u05E9\u05DC \u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05D1\u05D4\u05EA\u05D1\
  \u05E1\u05E1 \u05E2\u05DC \u05DB\u05DC\u05DC\u05D9\u05DD \u05E9\u05DE\u05D5\u05D2\
  \u05D3\u05E8\u05D9\u05DD \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D3\u05E4\u05D5\u05E1\
  \ (\u05DC\u05E8\u05D5\u05D1 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D1\u05D9\
  \u05D8\u05D5\u05D9\u05D9\u05DD\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05DE\
  \u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים התואמים דפוס מסוים היא על מנת להסיר תווים מסוימים או סדרות של תווים ממחרוזות, בהתבסס על כללים שמוגדרים על ידי דפוס (לרוב באמצעות ביטויים רגולריים). תכנתנים זקוקים תדיר לבצע משימה זו עבור ניקוי נתונים, הכנה לניתוח נתונים, עיצוב פלט או פשוט עריכת מחרוזות כדי לעמוד בדרישות היישום.

## איך לעשות:

ב-Go, מחיקת תווים התואמים דפוס יכולה להתבצע ביעילות באמצעות חבילת `regexp`. כאן, אנו נראה כיצד להסיר את כל הספרות, ואז את כל התווים שאינם אלפאנומריים ממחרוזת כדוגמאות.

1. **הסרת כל הספרות:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 is cool, but Go2 will be cooler! Now: 2023."
	
    // קמפול הביטוי הרגולרי עבור ספרות
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("שגיאה בקמפול הביטוי הרגולרי:", err)
        return
    }
	
    // החלפת ספרות במחרוזת ריקה
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // פלט: Go is cool, but Go will be cooler! Now: .
}
```

2. **הסרת כל התווים שאינם אלפאנומריים:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go is #1 @ programming languages!"
	
    // קמפול הביטוי הרגולרי עבור תווים שאינם אלפאנומריים
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("שגיאה בקמפול הביטוי הרגולרי:", err)
        return
    }
	
    // החלפת תווים שאינם אלפאנומריים במחרוזת ריקה
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // פלט: Gois1programminglanguages
}
```

## עיון מעמיק

חבילת `regexp` ב-Go מספקת ממשק חזק להתאמת דפוסים והתעסקות עם ביטויים רגולריים. ההוצאה לפועל שלה היא נגזרת מ-RE2, ספריית ביטויים רגולריים שתוכננה להבטיח ביצוע בזמן ליניארי, תוך מניעת האפשרות לבעיית "backtracking אסון" שנוכחת במנועי ביטויים רגולריים אחרים. זה הופך את regex של Go ליחסית בטוחה ויעילה למגוון רחב של שימושים.

למרות שחבילת `regexp` היא פתרון מקיף להתמודדות עם דפוסים, שווה לציין שלמניפולציות מחרוזת פשוטות יותר או מאוד פרטניות, פונקציות אחרות של מחרוזות כמו `strings.Replace()`, `strings.Trim()`, או חיתוך עשויות להציע אלטרנטיבות יותר ביצועיות. ביטויים רגולריים הם כלי חזק, אך העלות החישובית היחסית שלהם אומרת שלפעולות שניתן להגדיר ללא כלי זה, חקירת אלטרנטיבות של הספרייה הסטנדרטית לפעמים עשוייה להוביל לקוד פשוט ויעיל יותר.
