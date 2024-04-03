---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:22.513002-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05D4\u05EA\u05D7\u05DC\u05EA \u05E9\u05D9\u05D7\u05D4 \u05DE\
  \u05D9\u05D9\u05E9\u05D5\u05DD \u05D4-Go \u05E9\u05DC\u05DA \u05DC\u05E9\u05E8\u05EA\
  \ \u05D5\u05D5\u05D1, API, \u05D0\u05D5 \u05DB\u05DC \u05E9\u05D9\u05E8\u05D5\u05EA\
  \ \u05D0\u05D7\u05E8 \u05D4\u05DE\u05D1\u05D5\u05E1\u05E1 \u05E2\u05DC HTTP. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD \u05DE\u05E7\u05D5\u05E8\u05D5\
  \u05EA \u05D5\u05D5\u05D1,\u2026"
lastmod: '2024-03-13T22:44:38.484574-06:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05D4\u05EA\u05D7\u05DC\u05EA \u05E9\u05D9\u05D7\u05D4 \u05DE\
  \u05D9\u05D9\u05E9\u05D5\u05DD \u05D4-Go \u05E9\u05DC\u05DA \u05DC\u05E9\u05E8\u05EA\
  \ \u05D5\u05D5\u05D1, API, \u05D0\u05D5 \u05DB\u05DC \u05E9\u05D9\u05E8\u05D5\u05EA\
  \ \u05D0\u05D7\u05E8 \u05D4\u05DE\u05D1\u05D5\u05E1\u05E1 \u05E2\u05DC HTTP."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## מה ולמה?

שליחת בקשת HTTP כוללת התחלת שיחה מיישום ה-Go שלך לשרת ווב, API, או כל שירות אחר המבוסס על HTTP. מתכנתים עושים זאת כדי לאפשר אינטראקציה עם מקורות ווב, לאחזר נתונים, לשלוח טפסים, או לתקשר עם שירותים אחרים ברחבי האינטרנט.

## איך לעשות:

ב-Go, שליחת בקשת HTTP וטיפול בתגובה כוללים שימוש בחבילה `net/http`. הנה דוגמה צעד אחר צעד שמראה איך לשלוח בקשת GET פשוטה ולקרוא את התגובה:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // הגדרת כתובת URL של המשאב
    url := "http://example.com"

    // שימוש ב-http.Get לשליחת הבקשת GET
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // סגירת גוף התגובה כאשר הפונקציה מסתיימת
    defer resp.Body.Close()

    // קריאת גוף התגובה
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // המרת גוף התגובה למחרוזת והדפסתו
    fmt.Println(string(body))
}
```

פלט לדוגמה (מקוצר לצורך תמצית):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

כדי לשלוח בקשת POST עם נתוני טופס, תוכל להשתמש ב-`http.PostForm`:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // הגדרת כתובת ה-URL ונתוני הטופס
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // שליחת הבקשת POST עם נתוני הטופס
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // קריאה והדפסת התגובה
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## עיון עמוק

חבילת ה-`net/http` ב-Go מספקת דרך עוצמתית וגמישה לאינטראקציה עם שרתי HTTP. המבנה שלה משקף את הדגש של Go על פשטות, יעילות ואמינות. במקור, פונקציונליות כמו טיפול בנתוני JSON או XML דרשה יצירת גוף בקשה באופן ידני והגדרת כותרות מתאימות. ככל ש-Go התפתח, הקהילה פיתחה חבילות ברמה גבוהה יותר המפשטות משימות אלו, כמו `gorilla/mux` לניתוב ו-`gjson` למניפולציה של JSON.

אחד המאפיינים הבולטים של לקוח ה-HTTP של Go הוא השימוש שלו בממשקים ומבנים, כמו `http.Client` ו-`http.Request`, המאפשרים התאמה אישית נרחבת ובדיקות. למשל, ניתן לשנות את ה-`http.Client` כך שיתם זמנים קצובים לבקשות או לשמור על חיבורים פעילים לשיפור הביצועים.

חלופה נחשבת לאינטראקציות HTTP פשוטות יותר היא שימוש בספריות צד שלישי כמו "Resty" או "Gentleman". חבילות אלו מציעות אבסטרקציה ברמה גבוהה יותר לבקשות HTTP, המפשטות משימות נפוצות. עם זאת, להבין ולהשתמש בחבילת `net/http` הבסיסית חשוב לטיפול בתרחישי אינטראקציית HTTP מורכבים או ייחודיים יותר, תוך ספק שעליו ניתן לנצל במלואו את תכונות הבלתי חסימה והספרייה הסטנדרטית העוצמתית של Go.
