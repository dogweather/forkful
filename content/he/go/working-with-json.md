---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON הוא פורמט נתונים פופולרי לשיתוף בין שרתים ולקוחות. תכניתנים משתמשים בו כי הוא פשוט, קל לקריאה ונתמך ברוב השפות.

## איך לעשות:
קריאה ל-JSON:
```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID   int    `json:"id"`
    Name string `json:"name"`
}

func main() {
    jsonStr := `{"id": 1, "name": "משתמש A"}`
    var user User

    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatalf("Error occurred: %s", err)
    }

    fmt.Printf("%+v\n", user)
}
```
פלט:
```
{ID:1 Name:משתמש A}
```

כתיבה ל-JSON:
```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    user := &User{
        ID:   2,
        Name: "משתמש B",
    }

    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatalf("Error occurred: %s", err)
    }

    fmt.Printf("%s\n", userJSON)
}
```
פלט:
```
{"id":2,"name":"משתמש B"}
```

## ניתוח עמוק:
JSON, או JavaScript Object Notation, נוצר על ידי דאג קרוקפורד בתחילת שנות ה-2000. אלטרנטיבות כוללות XML וYAML, אך JSON הפך להרווח בזכות פשטותו וקלות השילוב עם JavaScript. בגולנג, החבילה `encoding/json` מאפשרת קריאה וכתיבה נוחה של JSON, עם המרה אוטומטית בין JSON למבני נתונים של גולאנג דרך תגיות במבנים (struct tags).

## ראו גם:
- מדריך JSON רשמי: https://www.json.org/json-en.html
- מסמכי החבילה `encoding/json` של גולאנג: https://pkg.go.dev/encoding/json
- מאמר על דאג קרוקפורד ויצירת JSON: https://www.crockford.com/json.html
