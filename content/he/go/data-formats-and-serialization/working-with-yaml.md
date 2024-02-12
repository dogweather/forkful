---
title:                "עבודה עם YAML"
aliases: - /he/go/working-with-yaml.md
date:                  2024-02-03T18:14:34.600715-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם YAML ב-Go כרוכה בפרסור קבצי YAML (YAML Ain't Markup Language), תקן סידורי נתונים ידידותי לאדם, לתוך מבני נתונים של Go ולהפך. מתכנתים עושים זאת כדי לנצל את הפשטות והקריאות של YAML עבור קבצי תצורה, הגדרות אפליקציה, או החלפת נתונים בין שירותים ורכיבים הכתובים בשפות שונות.

## איך ל:

כדי לעבוד עם YAML ב-Go, תצטרך להתקין קודם ספרייה התומכת בפרסור ובסידור של YAML, מכיוון שספריית הסטנדרט של Go אינה כוללת תמיכה ישירה ב-YAML. הספרייה הפופולרית ביותר למטרה זו היא "gopkg.in/yaml.v3". הנה איך להתחיל:

1. **התקנת חבילת YAML:**

```bash
go get gopkg.in/yaml.v3
```

2. **פרסור YAML למבנה Go:**

ראשית, יש להגדיר struct ב-Go התואם למבנה נתוני ה-YAML שלך.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**פלט לדוגמה:**

```
User: admin
Password: secret
```

3. **סידור מבנה Go ל-YAML:**

הנה איך להמיר מבנה Go בחזרה ל-YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**פלט לדוגמה:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## ירידה לעומק:

השימוש ב-YAML בפיתוח תוכנה התרחב בשל הפורמט הקריא לאדם, המהווה בחירה אידיאלית עבור קבצי תצורה, תיעוד, או פורמטים להחלפת נתונים. בהשוואה ל-JSON, הגרסה הנגדית שלו, YAML מציע הערות, סוגי סקלרים ותכונות יחסים, מה שמספק מסגרת סידורי נתונים עשירה יותר. עם זאת, הגמישות והתכונות שלו מגיעות במחיר של מורכבות בפרסור, המובילה לסיכוני אבטחה פוטנציאליים כאשר לא מטופלים בזהירות (למשל, ביצוע קוד שרירותי).

הספרייה "gopkg.in/yaml.v3" עבור Go היא פתרון עמיד עבור עיבוד YAML, המציע שיווי משקל בין נוחות שימוש ותמיכה נרחבת בתכונות. כפי שהמצב עכשיו, למרות שיש חלופות כמו "go-yaml/yaml" (הספרייה מאחורי "gopkg.in/yaml.v3"), הגרסה שנבחרת תלויה בדרך כלל בדרישות הפרויקט הספציפיות או בהעדפה האישית. כאשר מתמודדים עם סטי נתונים גדולים או אפליקציות קריטיות מבחינת ביצועים, מתכנתים עשויים לשקול פורמטים פשוטים יותר כמו JSON בשל זמני הפרסור ושימוש בזיכרון המופחתים שלהם. עם זאת, עבור קבצי תצורה או הגדרות שבהן קריאות האדם ונוחות השימוש הם העיקר, YAML נשאר מתחרה חזק באקוסיסטם של Go.
