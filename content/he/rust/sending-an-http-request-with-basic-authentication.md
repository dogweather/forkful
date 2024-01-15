---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Rust: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

 איך ל

## להרחיב

HTTP (פרוטוקול תעבורה משולת כמתשמש המאפשר לנו לתקשר עם שרתים באמצעות בקשות מקודדות. אחת מהבקשות הפופולריות ביותר היא בקשת GET, אשר מאפשרת לנו לקבל מידע משרת. אם אנו רוצים לאמת את הזהות שלנו בשרת, ניתן לעשות זאת באמצעות נתינת אישורים בסיסיים כחלק מהבקשה. למאמר זה יש כוונת ללמד אתכם כיצד לשלוח בקשת HTTP עם אימות בסיסי שיטות נפוצות שימוש בשפת ראסט.

## איך ל

ישנן כמה שיטות לבצע אימות בסיסי בתוך סביבת ראסט. בדוגמא זו, ננסה לשלוח בקשת GET עם אימות בשימוש בספריית hyper לצורך תרגול.

```Rust
// ייבוא הספרייה הנחוצה
𝜆𝜆𝜆𝜆 use hyper::Client;

fn main() {
    // הכנסת כתובת URL למשתנה
    let url = "https://example.com";

    // יצירת אובייקט Client ופניית בקשה GET עם אימות בסיסי
    let client = Client::new();
    let req = client.get(url).header(Authorization, "Basic dXNlcjpwYXNz").body(());

    // שליחת הבקשה וקבלת התגובה
    let mut res = req.send().unwrap();

    // הדפסת הגוף של התגובה לצורך אימות
    println!("{}", res.text().unwrap());
}
```

תוצאה:
```
response body
```

## להרחיב

בדוגמא זו, אנו משתמשים בספריית hyper כדי להתחבר לשרת עם בקשת GET אשר מכילה אימות בסיסי. נוסף לכך, אנו גם משתמשים בפניית בקשה עם כתובת URL ישירות, ולא באמצעות יצירת אובייקט Request נפרד. כאשר אנו משתמשים בפונקציה `.header()` כדי להוסיף אימות בסיסי לבקשה, נפריד את הנתונים הרלוונטים עם נקודתייים