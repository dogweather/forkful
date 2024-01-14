---
title:                "Rust: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

משתמשים רבים בשפת תכנות ראסט כדי לשלוח בקשות HTTP. בעזרת כך, ניתן לתקשר עם שרתים שונים ולקבל מידע חזרה באופן דינמי, לדוגמה תוצאות של בקשות ממסדי נתונים או מידע מכללת נתיבים. בנוסף, שליחת בקשות HTTP מראש תוכנן לקבלת מידע באופן אסינכרוני וחסכוני כך שהקוד לא ייתקע בזמן המתנה לתגובה.

## איך לעשות זאת

לפניכם נמצא דוגמא קוד לשליחת בקשות HTTP בשפת ראסט באמצעות ספריית Hyper. נתחיל על ידי יצירת פונקציה שתרצה לכלול בקוד שלכם את הקוד הבא:

```Rust
// ייבוא הספריה של Hyper
use hyper::{Client, Uri, Response, Body, StatusCode};
// ייבוא חבילת השגיאות של Hyper
use hyper::Error;
// ייצור משתנה שמייצג את כתובת האתר שלכם
let uri = "https://www.example.com".parse::<Uri>().unwrap();
// יצירת ספרייה חדשה של הלקוח
let client = Client::new();
// שליחת בקשה GET לכתובת האתר ולקבלת התשובה
let response: Response<Body> =client.get(uri).await?;
// הדפסת קוד המצב של התשובה
println!("{}", response.status());
// אם קוד המצב של התשובה הוא OK, הדפס את גוף התשובה
if response.status() == StatusCode::OK {
   let body = response.into_body();
   let full_body = body.fold(Vec::new(), |mut v, chunk| {
      v.extend_from_slice(&chunk);
      futures::future::ready(v)
   }).await;
   println!("Body: {:?}", full_body);
}
```

כעת, אתם יכולים להמיר ישירות את הקוד המופיע לעיל לקישורים ולהוסיף אותו לפונקציה הרלוונטית בקוד שלכם. יש לשים לב שכדי להשתמש בפונקציה הזו, תצטרכו לערוך את הספריית Cargo שלכם ולהוסיף את ההתחברויות הדרושות.

## ייצוג עמוק

כדי להבין מה ק