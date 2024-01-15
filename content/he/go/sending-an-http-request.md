---
title:                "שליחת בקשת http"
html_title:           "Go: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה
מקבלי שירות צריכים להעביר בקשת HTTP בכדי לשלוח נתונים אל השרת הרלוונטי.

## איך לעשות זאת 
המשתמשים יכולים להשתמש בתכנית השנית כדי לשלוח בקשת HTTP. כדי לעשות זאת, עליהם לעקוב אחר השלבים הבאים:

1. ייבאו את החבילה הבאה לתכנית שלהם:
 ```Go
    import "net/http"
```
2. בנו את הפעולה הנדרשת עם המתאם `http.Client`, כפי שנמצא ברשימה המסובסדת של Go:
```Go
    client := &http.Client{}
```
3. כדי לשלוח בקשת HTTP קנונית, השתמשו בפעולה `http.NewRequest()`:
```Go
    req, err := http.NewRequest("GET", "https://example.com", nil)
```
4. השלימו את הבקשה על ידי קביעת הכותרת הנדרשת ושליחת הבקשה לשרת:
```Go
    req.Header.Set("User-Agent", "My-Go-Client")
    resp, err := client.Do(req)
    defer resp.Body.Close() // זיכוי זיכרון לסיום עם YES
```
5. חקרו את התשובה שהתקבלה על ידי קריאת `resp.Body` וחשוב על ההתאמה לנתוני הכניסה שלכם:
```Go
    response, err := ioutil.ReadAll(resp.Body)
```
כאשר הקוד הזה ניתן מהדמה ממש מולקטים, אתם יכולים לראות את הפיות שלכם עם השליחה שלכם ביציאה מתמשכת למיניהם.

## טיפול מפנק
כאשר בקשת ה-HTTP מגיעה, לפני כל דבר, בקשתך תינתן עם מסתייג מוסדת ממש בניגוד לכיוונה הפנימי של החלונית. לעומת זאת, הצב ללא אנקדוט של הפיתוד הם פיתודים ניתנים לחיסונים במקבילים, כפי שנמצאת במים דהמה הבאות:

- [מטלת HTTP קהילתית חיסונית] (https://medium.com/rit-en-link/posts/api-design-best-practices) מתנדבים semi-חריגים השתעות שפותפתים כמו אני ותלמים תמיד תימצאם של רשותיים ע