---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:49.378857-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D9\u05E6\u05D9\
  \u05E8\u05D4 \u05D5\u05D0\u05EA\u05D7\u05D5\u05DC \u05E9\u05DC map \u05D1-Go \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05E8\u05DB\u05D9\u05DD\
  \ \u05E9\u05D5\u05E0\u05D5\u05EA. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4\
  \ \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05D4\u05EA\u05D7\u05DC\u05D4."
lastmod: '2024-03-13T22:44:38.477605-06:00'
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05D4 \u05D5\u05D0\u05EA\u05D7\u05D5\u05DC \u05E9\
  \u05DC map \u05D1-Go \u05E0\u05D9\u05EA\u05DF \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\
  \u05E8\u05DB\u05D9\u05DD \u05E9\u05D5\u05E0\u05D5\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## איך לעשות:
יצירה ואתחול של map ב-Go ניתן לבצע בדרכים שונות. הנה דוגמה בסיסית להתחלה:

```go
package main

import "fmt"

func main() {
    // הגדרה ואתחול של map
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // פלט: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

להוסיף או לעדכן אלמנטים, משים ערך למפתח כך:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// פלט: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

גישה לערך לפי מפתח היא פשוטה:

```go
fmt.Println("קוד ההקס של אדום הוא:", colors["red"])
// פלט: קוד ההקס של אדום הוא: #FF0000
```

למחוק אלמנט, משתמשים בפונקציה `delete`:

```go
delete(colors, "red")
fmt.Println(colors)
// פלט: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

לעבור על map נעשה באמצעות לולאת for:

```go
for color, hex := range colors {
    fmt.Printf("Key: %s Value: %s\n", color, hex)
}
```

זכור, maps ב-Go הם לא מסודרים. סדר העברה אינו מובטח.

## צלילה עמוקה
ב-Go, maps מיושמים בתור טבלאות גיבוב. כל רשומה ב-map מכילה שני פריטים: מפתח וערך. המפתח מעוגן כדי לאחסן את הרשומה, מה שמאפשר פעולות בזמן קבוע עבור קבוצה קטנה של נתונים ומורכבות זמן ממוצעת של O(1) עם גיבוב תקני, שיכולה להתדרדר ל-O(n) במקרה הגרוע עם התנגשויות גיבוב רבות.

הערה חשובה למתכנתי Go חדשים היא שסוגי ה-map הם סוגי ייחוס. זה אומר כאשר אתה מעביר map לפונקציה, כל שינוי שנעשה ל-map בתוך הפונקציה נראה לקורא. זה שונה מ, למשל, העברת מבנה לפונקציה, שם המבנה מועתק אלא אם כן מועבר על ידי מצביע.

בעוד ש-maps הם כלי גמיש ויעיל לרוב תרחישים הכוללים מערכי ערכים אסוציאטיביים, באפליקציות קריטיות בביצועים, ייתכן שיהיה כדאי להשתמש במבני נתונים עם תכונות ביצוע ניבויות יותר, במיוחד אם התפלגויות המפתח עלולות לגרום להתנגשויות תכופות.

אלטרנטיבה נוספת לשקול היא ה-`sync.Map`, זמין מאז Go 1.9, שנועד לתרחישים בהם מפתחות נכתבים פעם אחת אך נקראים פעמים רבות, מה שמציע שיפורים ביעילות בתרחישים אלו. עם זאת, ליישומים רגילים של Go, שימוש רגיל ב-map הוא השיטה המומלצת עקב פשטותו ותמיכה ישירה בשפה.
