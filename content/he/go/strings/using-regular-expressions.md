---
aliases:
- /he/go/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:57.059122-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05E9\u05DE\
  \u05E9\u05D9\u05DD \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9, \u05D4\u05EA\u05D0\u05DE\
  \u05D4, \u05D5\u05E2\u05D9\u05D1\u05D5\u05D3 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05EA\u05D1\u05E0\u05D9\
  \u05D5\u05EA \u05DE\u05E1\u05D5\u05D9\u05D9\u05DE\u05D5\u05EA. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\
  \u05DD \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05D4\u05D7\u05DC \u05DE\u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D9\u05DE\u05D5\u05EA \u05E4\u05E9\u05D5\
  \u05D8\u05D5\u05EA \u05D5\u05E2\u05D3\u2026"
lastmod: 2024-02-18 23:08:52.328083
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05E9\u05DE\
  \u05E9\u05D9\u05DD \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9, \u05D4\u05EA\u05D0\u05DE\
  \u05D4, \u05D5\u05E2\u05D9\u05D1\u05D5\u05D3 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05EA\u05D1\u05E0\u05D9\
  \u05D5\u05EA \u05DE\u05E1\u05D5\u05D9\u05D9\u05DE\u05D5\u05EA. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\
  \u05DD \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05D4\u05D7\u05DC \u05DE\u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D9\u05DE\u05D5\u05EA \u05E4\u05E9\u05D5\
  \u05D8\u05D5\u05EA \u05D5\u05E2\u05D3\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D9\u05DC\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) בתכנות משמשים לחיפוש, התאמה, ועיבוד מחרוזות בהתבסס על תבניות מסויימות. מתכנתים משתמשים בהם למשימות החל מבדיקות אימות פשוטות ועד לעיבוד טקסט מורכב, הופכים אותם לבלתי ניתנים לוויתור לטיפול בטקסט באופן גמיש ויעיל.

## איך לעשות:

ב-Go, החבילה `regexp` מספקת פונקציונליות ביטויים רגולריים. הנה מדריך צעד אחר צעד איך להשתמש בה:

1. **קימופילציה של ביטוי רגולרי**

תחילה, קמפלו את תבנית ה-regex שלכם באמצעות `regexp.Compile`. זהו מנהג טוב לטפל בשגיאות שעשויות להתעורר במהלך ההידור.

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    pattern := "go+"
    r, err := regexp.Compile(pattern)
    if err != nil {
        fmt.Println("Error compiling regex:", err)
        return
    }
    
    fmt.Println("Regex compiled successfully")
}
```

2. **התאמה של מחרוזות**

בדקו אם מחרוזת מתאימה לתבנית באמצעות המתודה `MatchString`.

```go
matched := r.MatchString("goooooogle")
fmt.Println("Matched:", matched) // פלט: Matched: true
```

3. **מציאת התאמות**

כדי למצוא את ההתאמה הראשונה במחרוזת, השתמשו במתודה `FindString`.

```go
match := r.FindString("golang gooooo")
fmt.Println("Found:", match) // פלט: Found: gooooo
```

4. **מציאת כל ההתאמות**

לכל ההתאמות, `FindAllString` לוקחית מחרוזת קלט ומספר n. אם n >= 0, היא מחזירה לכל היותר n התאמות; אם n < 0, היא מחזירה את כל ההתאמות.

```go
matches := r.FindAllString("go gooo gooooo", -1)
fmt.Println("All matches:", matches) // פלט: All matches: [go gooo gooooo]
```

5. **החלפת התאמות**

כדי להחליף התאמות עם מחרוזת אחרת, `ReplaceAllString` יכולה לעזור.

```go
result := r.ReplaceAllString("go gooo gooooo", "Java")
fmt.Println("Replaced:", result) // פלט: Replaced: Java Java Java
```

## צלילה עמוקה

החבילה `regexp`, המוצגת בספריית התקן של Go, מיישמת חיפוש ביטויים רגולריים והתאמת תבניות בהשראת תחביר Perl. מתחת לכיפה, מנוע ה-regex של Go מקמפל את התבניות לצורה של bytecode-ים, אשר לאחר מכן מופעלים על ידי מנוע ההתאמה שנכתב ב-Go עצמו. המימוש מפקיר מעט מהמהירות הנמצאת בביצוע חומרה ישיר בשם הבטיחות והנוחות של השימוש, תוך הימנעות מהמלכודות של עודפי באפר הנפוצים בספריות מבוססות C.

למרות כוחו, regex ב-Go לא תמיד הוא הפתרון האופטימלי להתאמת תבניות, במיוחד כאשר מתמודדים עם נתונים מאוד מובנים כמו JSON או XML. במקרים אלו, מפענחים מיוחדים או ספריות מותאמות לפורמטי נתונים אלה מציעות ביצועים ואמינות טובים יותר. עם זאת, למשימות הכרוכות בעיבוד טקסט מורכב ללא מבנה מוגדר מראש, regex נותר כלי חיוני בארגז הכלים של מתכנת, ומציע איזון של כוח וגמישות שמעט חלופות יכולות להתמודד איתו.
