---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## מָה ולָמָה?
פירוז HTML הוא התהליך בו מפענחים ומנתחים מידע ממסמכי HTML. מתכנתים עשויים לתקל בצורך לבצע פירוז HTML כדי להפיק מידע בר-שימוש מקבצים או מקורות חיצוניים.

## איך לעשות זאת:
הנה דוגמא של איך להשתמש בגלים לניתוח HTML.
```Gleam
import gleam/http.{Method, Uri}
import gleam/httpc

fn main() {
    let response = httpc.send(Method.Get, Uri.from_string("http://example.com").unwrap())
    let html = response.body.to_string()  
    println(html)      
}
```
כאשר תריצו את קטע הקוד הזה, תקבלו את כל ה-HTML של האתר כדי לנתח.

## צניחה עמוקה
ניתוח HTML הוא תהליך עתיק, ואכן הוא ממשיך להשתמש באופן נרחב. ניתן להשתמש במיני דרכים רבות, כולל מספר של אלטרנטיבות כמו XML או JSON. מרבית סיפריות הניתוח מחזירות מסד נתונים לייצוג המסמך, דרכו הפרטים במסמך המקורי בדרך כלל ניתנים לחיפוש באופן מבני. Gleam, כמו רוב השפות, משתמשת בספריות חיצוניות כדי לאפשר את פירוז HTML.

## עיין גם
עיין במקורות הבאים למידע נוסף על ניתוח HTML:
- [HTML5 parsing techniques and specifications](https://html.spec.whatwg.org/multipage/parsing.html)

#### בלי סיכום:
במאמר זה, דיברנו על הגדרת ניתוח HTML, הסיבות לניתוח HTML, כיצד לנתח HTML באמצעות גלים, ופרטים נוספים. אין לנו "סיכום" במאמר זה כי אנו רוצים לשמור על שפה קצרה וברורה.