---
title:                "תוכנה מחשבית: שימוש בביטויים רגילים."
html_title:           "Go: תוכנה מחשבית: שימוש בביטויים רגילים."
simple_title:         "תוכנה מחשבית: שימוש בביטויים רגילים."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מדוע

פקודות הכוונה רגילות (Regular expressions) הן כלי חזק שנמצא בשימוש רחב לפתרון בעיות נוספות בקוד, כגון סינכרון דטא, ולשינוי בתבניות של מחרוזות. אם אתה מתכנת גו או מעוניין להתחיל לתכנת בגו, יש לך את הארגשה הנכונה.

## כיצד להשתמש בפקודות הכוונה רגילות

פקודות הכוונה רגילות נמצאות בתוך החבילה של "regexp", ויש לייבא אותה בקוד על ידי הוספת הנקודת פתיחה `.` לפני השם של החבילה. כאן אתה יכול לראות דוגמאות בגו ואת הפונקציות הנכונות בבלוקי קוד.

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Complie pattern for an email address
    pattern := regexp.MustCompile("^[a-zA-Z0-9.+_-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z]+$")
    // Test if string is a valid email
    result := pattern.MatchString("example@email.com")
    fmt.Println(result) // Output: true
}
```

## כיוון מעמיק בפקודות הכוונה רגילות

כאשר משתמשים בפקודות הכוונה רגילות, יש לקחת בחשבון כמה דברים כדי לדייק ולמנוע שגיאות. נהוג להשתמש בפונקציות כגון `regexp.MatchString()` לבדיקה של מחרוזת או `regexp.FindString()` להתאמות נוספות. בנוסף, כדי לחזור על ביטוי מקסימלי במחרוזת, ניתן להשתמש בפונקציות כגון `regexp.FindAllString()` או `regexp.FindAllStringSubmatch()`. אנחנו מקווים שמאמר זה עזר לך להבין את השימוש בפקודות הכוונה רגילות ולהתחיל להשתמש בהן בעבודתך בגו.

## ראה גם

למידע נוסף על פקודות הכוונה רגילות בגו, הצטרף לקבוצת הדיון שלנו ברשת Reddit ועיין במדריך המפורט [כאן](https://