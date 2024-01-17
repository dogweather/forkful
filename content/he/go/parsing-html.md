---
title:                "ניתוח HTML"
html_title:           "Go: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML בשפת גוגל גו

## מה ולמה?

פארסינג של HTML היא התהליך שבו מתוכנתים נכנסים לתוך דף אינטרנט, קוראים את הקוד שלו ויוצאים בעלת מידע מועיל על הדף. תהליך זה חשוב למתכנתים כדי לאפשר להם לשלב נתונים מועילים מדפי אינטרנט ולמאפיין אותם לניתוח ועיבוד.

## איך לעשות זאת:

התכנית הבאה מדגימה את השימוש בספריית הפניהור כדי לפרסם ולהדפיס את התוכן של דף אינטרנט:
```Go
import (
    "fmt"
    "net/http"
    "github.com/PuerkitoBio/goquery"
)

func main() {
    response, _ := http.Get("https://examplewebsite.com")
    defer response.Body.Close()
    document, _ := goquery.NewDocumentFromReader(response.Body)
    
    document.Find("h1").Each(func(index int, element *goquery.Selection) {
        fmt.Println(element.Text())
    })
}
```
פלט התוכנית ישיג את כותרת הדף מהתגית h1 וידפיס אותה בטקסט נקי.

## כיול עמוק:

פירוט קצר של מה פעילות הנדסת HTML מיישמת בתכנות השלב בפיתוח הווב, מספר פתרונות ספציפיים שאפשר להשתמש בהם במקום הפניהור וכיצד הוא מתממש.

מעבר לספריית הפניהור, ישנם פתרונות אחרים שניתן להשתמש בהם כדי לפרסם דפים אינטרנט בגוגל גו, כגון הספרייה המדורגת גיני. בנוסף, חשוב לציין כי גוגל גו מציע תמיכה מלאה בכתיבת אפליקציות אינטרנט המשתמשות ב HTML כאשר ההתאמה לעולם הווב הינה תמיד מהירה ויעילה.

## ראה גם:

- דוקומנטציה רשמית של ספריית הפניהור של גוגל גו: https://godoc.org/github.com/PuerkitoBio/goquery
- ערוץ האינטרנט של גוגל גו עם תוכן מגוון על פיתוח ותכנות: https://www.youtube.com/channel/UC0NErq0RhP51iXx64ZmyVfg