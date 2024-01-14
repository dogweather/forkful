---
title:                "Rust: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

מתי יש לשלוח בקשת HTTP עם אימות בסיסי וכמה נוח לכלול את התהליך הזה בתוך קוד ראסט.

## איך לעשות

כאשר אנו משתמשים באתר מסוים, לעתים קרובות אנו נדרשים להתחבר כדי לגשת לחלקיו שאינם ציבוריים. כדי לעשות זאת, אנו נשלח בקשת HTTP עם כותרת אימות בסיסית. כאן נמצאת דוגמת קוד ראסט פשוטה המדגימה איך לשלוח בקשת HTTP עם אימות בסיסי:

```Rust
// מימוש פונקציה המקבלת את הכתובת והמידע על האימות
fn send_basic_auth_request(url: &str, username: &str, password: &str) {
    // ייבוא התלבושת הרלוונטית
    use reqwest::{Client, Error};
    use std::collections::HashMap;

    // יצירת אובייקט המבוסס על שם המשתמש והסיסמה
    let credentials = format!("{}:{}", username, password);
    // יצירת טבלת חיפוש המכילה את האימות בסיסי
    let mut headers = HashMap::new();
    headers.insert("Authorization", format!("Basic {}", base64::encode(credentials)));

    // שליחת הבקשה לכתובת הצויינת עם תווך האימות
    let client = Client::new();
    let res = client.get(url)
        .headers(headers)
        .send();

    // הדפסת תוצאת הבקשה
    match res {
        // אם אין שגיאות, תוצאת הבקשה תיחזר כמו כן
        Ok(response) => println!("{}", response.text().unwrap()),
        // אחרת, יוכןס שגיאה המציינת את סיבת הכשלון
        Err(err) => println!("אירעה שגיאה: {:?}", err),
    }
}

// קריאה לפונקציה ופעולת השליחה
fn main() {
    let url = "http://www.example.com";
    let username = "myusername";
    let password = "mypassword";
    send_basic_auth_request(url, username, password);
}
```

תוצאה:

```
<!DOCTYPE html>
<html>
<head>
    <title>Welcome to Example</title>
</head>
<body>
    <h1>Welcome to Example</h1>
    <p>You have successfully logged in!</p>
</body>
</html>
```

## בירור עמוק

אימות בסיסי הוא תהליך אבטחתי שמשמש למתן גישה לחלקי האתר שאינם ציבוריים. תוכניות ה-HTTP מאפשר