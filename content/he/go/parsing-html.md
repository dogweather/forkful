---
title:                "ניתוח HTML"
date:                  2024-01-20T15:32:04.939461-07:00
simple_title:         "ניתוח HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח (Parsing) של HTML הוא תהליך שבו אנו מעבדים מסמך HTML כדי להבין את מבנהו ותוכנו. מפתחים עושים זאת כדי לקרוא, לערוך, או לאסוף נתונים מדפי אינטרנט.

## איך לעשות:
גו (Go) מספקת ספרייה מובנית בשם `net/html` שתעזור לנו לפרס דפי HTML. להלן דוגמה פשוטה:

```Go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    resp, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    doc, err := html.Parse(resp.Body)
    if err != nil {
        panic(err)
    }

    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }
    f(doc)
}
```

תוצאת דוגמה:

```
http://www.iana.org/domains/example
```

## צלילה עמוקה
בעבר, פיענוח HTML היה מאתגר בשל מגוון המבנים והתקנים. היום, עם כלים כמו `net/html` בגו, התהליך פשוט יותר. ישנם גם חלופות כמו `goquery` שמדמה את jQuery. בעת ביצוע פיענוח, חשוב לזכור ש HTML אינו תמיד תקני וכלים צריכים להיות עמידים בפני שגיאות.

## ראה גם
- דוקומנטציית `net/html`: https://pkg.go.dev/golang.org/x/net/html
- מדריך לספריה `goquery`: https://github.com/PuerkitoBio/goquery
- איך להשתמש ב`goquery` לפרסינג של HTML: https://www.devdungeon.com/content/web-scraping-go-using-goquery
