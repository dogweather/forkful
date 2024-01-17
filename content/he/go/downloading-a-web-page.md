---
title:                "הורדת דף אינטרנט"
html_title:           "Go: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה זו ולמה?

הורדת דף אינטרנט היא פעולה פשוטה שמאפשרת לקבל את התוכן של דף אינטרנט מהשרת ולהציגו על המסך. תהליכו מאפשר לנו לקבל מידע ממקור חיצוני ולהשתמש בו ביישומים שונים, כגון בניית אתר אינטרנט או יישום דומה.

## כיצד לעשות זאת:

כדי להוריד דף אינטרנט בשפת Go, ניתן להשתמש בפונקציית Get שנמצאת בחבילת "net/http". ניתן להעביר לפונקציה כתובת אינטרנט והיא תחזיר את תוכן הדף במקרה של הצלחה. הנה דוגמה לקוד ולפלט שלה:

```Go
package main

import (
    "fmt"
    "net/http"
)

func main() {
    resp, err := http.Get("https://www.google.com")
    if err != nil {
        fmt.Println(err)
    }
    fmt.Println(resp)
}
```

פלט:

```Go
&{200 OK 200 HTTP/1.1 1 1 map[Alt-Svc:[quic=":443"; ma=2592000; v="46,43",h3-Q050=":443"; ma=2592000,h3-Q049=":443"; ma=2592000,h3-Q048=":443"; ma=2592000,h3-Q046=":443"; ma=2592000,h3-Q043=":443"; ma=2592000] Cache-Control:[private, max-age=0] Content-Type:[text/html; charset=ISO-8859-1] Date:[Mon, 28 Jun 2021 12:12:23 GMT] Expires:[-1] P3p:[CP="This is not a P3P policy! See g.co/p3phelp for more info."] Server:[gws] Set-Cookie:[1P_JAR=2021-06-28-12; expires=Wed, 28-Jul-2021 12:12:23 GMT; path=/; domain=.google.com; Secure NID=219=zgF--E1U7BmxLeYEw-uwd36Y_YzRv9Rj6cn7aDGzE46JWhZd-A; expires=Tue, 28-Dec-2021 12:12:23 GMT; path=/; domain=.google.com; HttpOnly] X-Frame-Options:[SAMEORIGIN] X-Xss-Protection:[0]] 0xc0000cb000 -1 [chunked] false false}
```

## חפירה עמוקה:

התמיכה בפעולת הורדת דף אינטרנט ישנה מאוד ונמצאת במרכז הפעילות של כל תוכנית רשת. בכוונת היוצרים של שפת Go לספק כלי יעיל וקל לשימוש כדי לאפשר תמיכה מלאה בתקשורת רשת. נוסף לכך, ישנן אלטרנטיבות אחרות כגון החבילה "net/url" שמאפשרת לנו לבצע פעולות נוספות על כתובות אינטרנט.

## ראו גם:

למידע נוסף על הורדת דף אינטרנט בשפת Go ניתן לקרוא באתר הרשמי של גוגל גוד, בכתובת: https://golang.org/pkg/net/http/ או בבלוג הרשמי של גוגל גוד, בכתובת: https://blog.golang.org/go-slices-usage-and-internals.