---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:29.662213-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05D1-Go \u05DB\u05D5\u05DC\u05DC\
  \ \u05D0\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\
  \ \u05E7\u05D1\u05E6\u05D9 HTML \u05DB\u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\
  \u05DE\u05D1\u05E0\u05D4, \u05D0\u05D5 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA\
  \ \u05D4-HTML \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05D0\u05D7\u05E8\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DC\u05E9\u05DD \u05D2\u05E8\u05D9\u05E4\u05EA \u05D0\
  \u05EA\u05E8\u05D9\u05DD,\u2026"
lastmod: '2024-03-13T22:44:38.486206-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05D1-Go \u05DB\u05D5\u05DC\u05DC \u05D0\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D5\u05DB\u05DF \u05E9\u05DC \u05E7\
  \u05D1\u05E6\u05D9 HTML \u05DB\u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\u05DE\
  \u05D1\u05E0\u05D4, \u05D0\u05D5 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\
  -HTML \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05D0\u05D7\u05E8\u05D9\u05DD\
  ."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

## איך לעשות:
כדי לנתח HTML ב-Go, בדרך כלל משתמשים בחבילת `goquery` או בחבילת `net/html` של הספרייה הסטנדרטית. הנה דוגמא בסיסית בה משתמשים ב-`net/html` כדי לחלץ את כל הקישורים מדף אינטרנט:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // קבלת מסמך HTML
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // ניתוח המסמך HTML
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // פונקציה לטיול רקורסיבי ב-DOM
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

    // טיול ב-DOM
    f(doc)
}
```

פלט לדוגמא (בהנחה ש-`http://example.com` מכיל שני קישורים):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

הקוד הזה מבקש דף HTML, מנתח אותו, וטייל באופן רקורסיבי ב-DOM כדי למצוא ולהדפיס את תכונות ה-`href` של כל תגיות ה-`<a>`.

## היכנסות לעומק
חבילת `net/html` מספקת את היסודות לניתוח HTML ב-Go, על ידי מימוש ישיר של אלגוריתמי הטוקניזציה ובניית העץ המפורטים על ידי התקן HTML5. גישה זו ברמה נמוכה היא חזקה אך עשויה להיות מילולית למשימות מורכבות.

לעומת זאת, חבילת הצד השלישי `goquery`, המושראת מ-jQuery, מציעה ממשק ברמה גבוהה יותר המפשט את התמצאות והעברה ב-DOM. היא מאפשרת למפתחים לכתוב קוד תמציתי ובעל ביטוי עבור משימות כמו בחירת אלמנטים, חילוץ תכונות ושינוי תוכן.

עם זאת, הנוחות שמציעה `goquery` באה על חשבון תלות נוספת וביצועים פוטנציאלית איטיים יותר בגלל שכבת ההפשטה שלה. הבחירה בין `net/html` לבין `goquery` (או ספריות ניתוח אחרות) תלויה בדרישות הספציפיות של הפרויקט, כגון הצורך באופטימיזציה של ביצועים או נוחות שימוש.

היסטורית, ניתוח HTML ב-Go התפתח מפעולות עיבוד מחרוזות בסיסיות לשינוי מורכב של עצי DOM, משקף את האקוסיסטם הגדל של השפה ואת דרישת הקהילה לכלים חזקים לגריפת אתרים וחציבת נתונים. למרות היכולות המקוריות, השימוש הנרחב בספריות צד שלישי כמו `goquery` מדגיש את העדפת הקהילה של Go לקוד מודולרי וניתן לשימוש חוזר. עם זאת, ליישומים קריטיים בביצועים, מתכנתים עשויים עדיין להעדיף את חבילת `net/html` או אף להיעזר בביטויים רגולריים למשימות ניתוח פשוטות, תוך שמירה על הסיכונים והמגבלות הכרוכות בניתוח HTML באמצעות ביטויים רגולריים.
