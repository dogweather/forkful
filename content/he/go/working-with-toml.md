---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:23:00.241079-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם TOML כוללת ניתוח וקידוד של קבצי TOML (Tom's Obvious, Minimal Language) בשפת Go. תכנתים בוחרים ב-TOML בגלל הקריאות והמיפוי הקל למבני נתונים, התואם בצורה מוצקה לקובצי תצורה.

## איך לעשות זאת:
על מנת לעבוד עם TOML ב-Go, בדרך כלל יש להשתמש בספריה כמו `BurntSushi/toml`. הנה מבט מהיר על ניתוח קובץ תצורה של TOML:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

דוגמה ל-`config.toml`:

```Toml
title = "Example TOML"
[owner]
name = "Tom Preston-Werner"
```

דוגמה לפלט:

```
Title: Example TOML, Owner: Tom Preston-Werner
```

## צלילה עמוקה
TOML, שהוצג על ידי טום פרסטון-ורנר בשנת 2013, נוצר במטרה להיות פורמט קובץ תצורה מינימלי שקל לקרוא בזכות הסמנטיקה הברורה שלו. פיתוחי Go לעיתים קרובות משתמשים ב-TOML לתצורה על פני אלטרנטיבות כמו JSON או YAML בגלל הישירות והיכולת שלו לייצג היררכיות מורכבות בפשטות.

בהשוואה ל-YAML, שיש לו תכונות מורכבות ודאגות אבטחה אפשריות, עיצוב הדירה של TOML מפחית סיבוך וטעויות הנגרמות מהקלדה שגויה. ובניגוד ל-JSON, TOML תומך בהערות, מה שהופך אותו לקל יותר להסביר תצורות בתוך הקוד.

כשעובדים עם TOML ב-Go, יש מגבלות שכדאי להתחשב בהן. תגיות מבנה יכולות להתאים אישית איך המבנים שלך מתמפים למבני TOML, וכן כדאי להיות מודעים לאופן בו מערכי TOML וטבלאות inline נפרשים לחתיכות ומפות של Go.

## ראה גם
- מפרט TOML: https://toml.io/en/
- ספריית BurntSushi/toml: https://github.com/BurntSushi/toml
- השוואה של פורמטים לקובץ תצורה: https://www.redhat.com/sysadmin/yaml-toml-json-differences
