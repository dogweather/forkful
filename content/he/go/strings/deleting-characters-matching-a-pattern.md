---
title:                "מחיקת תווים שמתאימים לתבנית"
aliases:
- /he/go/deleting-characters-matching-a-pattern.md
date:                  2024-02-03T17:56:04.775098-07:00
model:                 gpt-4-0125-preview
simple_title:         "מחיקת תווים שמתאימים לתבנית"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
