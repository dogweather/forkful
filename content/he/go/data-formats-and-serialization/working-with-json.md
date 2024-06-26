---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:42.033599-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Go, \u05D4\
  \u05D7\u05D1\u05D9\u05DC\u05D4 `encoding/json` \u05D4\u05D9\u05D0 \u05D4\u05E9\u05E2\
  \u05E8 \u05E9\u05DC\u05DA \u05DC\u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\
  \u05D4 \u05E9\u05DC JSON, \u05D5\u05D4\u05D9\u05D0 \u05DE\u05E1\u05E4\u05E7\u05EA\
  \ \u05D0\u05DE\u05E6\u05E2\u05D9\u05DD \u05DC\u05D4\u05DE\u05E8\u05EA \u05DE\u05D1\
  \u05E0\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC Go \u05DC-JSON (\u05D4\
  \u05D8\u05DE\u05E2\u05D4) \u05D5\u05D7\u05D6\u05E8\u05D4 (\u05D1\u05D9\u05D8\u05D5\
  \u05DC \u05D4\u05D8\u05DE\u05E2\u05D4).\u2026"
lastmod: '2024-03-13T22:44:38.528795-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Go, \u05D4\u05D7\u05D1\u05D9\u05DC\u05D4 `encoding/json` \u05D4\u05D9\
  \u05D0 \u05D4\u05E9\u05E2\u05E8 \u05E9\u05DC\u05DA \u05DC\u05DE\u05E0\u05D9\u05E4\
  \u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC JSON, \u05D5\u05D4\u05D9\u05D0 \u05DE\
  \u05E1\u05E4\u05E7\u05EA \u05D0\u05DE\u05E6\u05E2\u05D9\u05DD \u05DC\u05D4\u05DE\
  \u05E8\u05EA \u05DE\u05D1\u05E0\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\
  \u05DC Go \u05DC-JSON (\u05D4\u05D8\u05DE\u05E2\u05D4) \u05D5\u05D7\u05D6\u05E8\u05D4\
  \ (\u05D1\u05D9\u05D8\u05D5\u05DC \u05D4\u05D8\u05DE\u05E2\u05D4)."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## איך לעשות:
ב-Go, החבילה `encoding/json` היא השער שלך למניפולציה של JSON, והיא מספקת אמצעים להמרת מבני נתונים של Go ל-JSON (הטמעה) וחזרה (ביטול הטמעה). להלן דוגמאות בסיסיות להתחלה:

### קידוד (הטמעה)
להמיר מבנה של Go ל-JSON, ניתן להשתמש ב-`json.Marshal`. בחן את המבנה של Go הבא:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

פלט:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### פענוח (ביטול הטמעה)
לפרסר JSON למבנה נתונים של Go, השתמש ב-`json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

בהינתן מבנה `User` כפי שהיה קודם, קוד זה מפרש את המחרוזת JSON למופע של משתמש.

פלט:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## צלילה עמוקה
חבילת ה-`encoding/json` ב-Go מציעה API ישיר שמפשטת הרבה מהמורכבות הכרוכה במניפולציה של JSON. הוצגה מוקדם בפיתוח Go, חבילה זו משקפת את פילוסופיית Go של פשטות ויעילות. עם זאת, השימוש ברפלקסיה על ידי `encoding/json` לבדיקה ושינוי מבנים בזמן ריצה יכול להוביל לביצועים לא אופטימליים בסצנריות שדורשות מעבד רב.

חלופות כמו `json-iterator/go` ו-`ffjson` הופיעו, ומספקות עיבוד JSON מהיר יותר על ידי יצירת קוד הטמעה וביטול הטמעה סטטי. עם זאת, `encoding/json` נשארת החבילה הנפוצה ביותר בשימוש בזכות הפשטות שלה, קושיות והעובדה שהיא חלק מהספרייה הסטנדרטית, מה שמבטיח תאימות ויציבות לאורך גרסאות של Go.

למרות הביצועים היחסית איטיים יותר, קלות השימוש והאינטגרציה עם מערכת הסוגים של Go מקנים ל-`encoding/json` התאמה לרוב היישומים. עבור אלו העובדים בהקשרים שבהם הביצועים מהווים שיקול עליון, חקירת ספריות חיצוניות עשויה להיות מועילה, אך לרבים, הספרייה הסטנדרטית מציעה את האיזון הנכון בין מהירות, פשטות, ואמינות.
