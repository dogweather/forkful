---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:44:22.663183-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא תהליך שבו אנחנו משיגים את תוכן דף האינטרנט בצורת טקסט. תוכניתנים עושים זאת לניתוח נתונים, בדיקות אוטומטיות ואינטגרציה עם שירותים שונים.

## איך לעשות:
נשתמש בחבילת `net/http` כדי להוריד את תוכן הדף. ראה דוגמה פשוטה:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    resp, err := http.Get("http://example.com")
    if err != nil {
        fmt.Println("Error fetching page:", err)
        return
    }
    defer resp.Body.Close()

    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        fmt.Println("Reading body error:", err)
        return
    }

    fmt.Println(string(body))
}
```
פלט לדוגמה:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## עיון נוסף:
תוך שמירה על התמציתיות, כמה פרטים עשויים להיות רלוונטיים לדעת. ההיסטוריה של גולאנג מראה שהוספת תמיכה ברשת הייתה חשובה מההתחלה. ישנן חבילות חלופיות כמו `golang.org/x/net/html` שמאפשרות ניתוח תוכן דפים. בעת שימוש ב-`http.Get`, חשוב לזכור תמיד לסגור את ה-`Body` כדי למנוע דליפות זיכרון.

## ראה גם:
- מדריך רשמי ל-`net/http` בגולאנג: https://golang.org/pkg/net/http/
- חבילת ניתוח HTML בגולאנג: https://pkg.go.dev/golang.org/x/net/html
- Go by Example – HTTP Clients: https://gobyexample.com/http-clients
