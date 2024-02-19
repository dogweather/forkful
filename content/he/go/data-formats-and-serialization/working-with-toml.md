---
aliases:
- /he/go/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:37.934215-07:00
description: "TOML (Tom's Obvious, Minimal Language - \u05E9\u05E4\u05EA \u05D4\u05D2\
  \u05D3\u05E8\u05D5\u05EA \u05DE\u05D9\u05E0\u05D9\u05DE\u05DC\u05D9\u05EA \u05D5\
  \u05D1\u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D8\u05D5\u05DD) \u05D4\u05D9\u05D0\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05D5\u05D1\u05E5 \u05D4\u05D2\u05D3\u05E8\
  \u05D5\u05EA \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D1\u05D6\u05DB\
  \u05D5\u05EA \u05EA\u05D7\u05D1\u05D9\u05E8 \u05E4\u05E9\u05D5\u05D8. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  -TOML \u05DC\u05D4\u05D2\u05D3\u05E8\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.367191
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language - \u05E9\u05E4\u05EA \u05D4\u05D2\u05D3\
  \u05E8\u05D5\u05EA \u05DE\u05D9\u05E0\u05D9\u05DE\u05DC\u05D9\u05EA \u05D5\u05D1\
  \u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D8\u05D5\u05DD) \u05D4\u05D9\u05D0 \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05E7\u05D5\u05D1\u05E5 \u05D4\u05D2\u05D3\u05E8\u05D5\
  \u05EA \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D1\u05D6\u05DB\u05D5\
  \u05EA \u05EA\u05D7\u05D1\u05D9\u05E8 \u05E4\u05E9\u05D5\u05D8. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-TOML\
  \ \u05DC\u05D4\u05D2\u05D3\u05E8\u05EA\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD TOML"
---

{{< edit_this_page >}}

## מה ולמה?

TOML (Tom's Obvious, Minimal Language - שפת הגדרות מינימלית וברורה של טום) היא פורמט קובץ הגדרות שקל לקרוא בזכות תחביר פשוט. מתכנתים משתמשים ב-TOML להגדרת הגדרות ותלות של יישומים בשל בהירותו והתאמה ישירה למבני נתונים, דבר שהופך אותו לבחירה פופולרית בפרויקטים רבים של Go להגדרה וניהול תצורות.

## איך לעשות:

כדי להתחיל לעבוד עם TOML ב-Go, קודם כל צריך לכלול ספרייה שיכולה לנתח קבצי TOML, מאחר והספרייה הסטנדרטית של Go אינה תומכת ב-TOML כברירת מחדל. החבילה `BurntSushi/toml` היא בחירה פופולרית לצורך זה. קודם כל, וודאו להתקין אותה:

```bash
go get github.com/BurntSushi/toml
```

הנה דוגמה פשוטה לשימוש בה. נניח שיש לכם קובץ הגדרות בשם `config.toml` עם התוכן הבא:

```toml
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

עכשיו, תצטרכו ליצור מבנה Go שמשקף את מבנה ה-TOML:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

דוגמא לפלט:

```
Title: TOML Example
Database Server: 192.168.1.1
```

## טבילה עמוקה

TOML נוצר על ידי טום פרסטון-ורנר, אחד ממייסדי GitHub, במטרה להציע פורמט קובץ הגדרות ישיר שניתן למפות בקלות לטבלת גיבוב ולהבינו במבט ראשון ללא ידע קודם של הפורמט. זה בניגוד ל-JSON או YAML, שבעוד שהם גם נמצאים בשימוש נרחב, יכולים להיות פחות ידידותיים לאדם עבור קבצי הגדרות בגלל בעיות של סוגריים, מרכאות והזחות.

החבילה `BurntSushi/toml` ב-Go היא ספרייה אמינה שלא רק מאפשרת ניתוח אלא גם קידוד של קבצי TOML, מה שהופך אותה לבחירה מגוונת עבור יישומים שצריכים לקרוא וגם לכתוב קבצי הגדרות בפורמט זה. עם זאת, יש לציין שעם התקדמות הטכנולוגיות והוצאת גרסאות חדשות של Go, עלו חלופות כמו `pelletier/go-toml`, המציעות ביצועים משופרים ותכונות נוספות כמו מניפולציה ותמיכה בשאילתות של עץ.

למרות ש-TOML הוא בחירה מעולה ליישומים רבים, בהתאם למורכבות של ההגדרות והעדפות אישיות או של הצוות, פורמטים אחרים כמו YAML או JSON יכולים להתאים יותר, במיוחד אם ההגדרה דורשת מבני נתונים יותר מורכבים שהטבע המפורט של TOML עשוי שלא לתפוס בחן. עם זאת, עבור הגדרות ישירות, קריאות וניתנות לעריכה בקלות, TOML, בשילוב עם מערכת הטיפוסים החזקה של Go והספריות הנ"ל, הוא בחירה מעולה.
