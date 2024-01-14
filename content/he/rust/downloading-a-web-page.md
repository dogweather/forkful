---
title:                "Rust: להורדת דף אינטרנט"
simple_title:         "להורדת דף אינטרנט"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

למה עשה אדם כלשהו יורד דף אינטרנט? רובם עושים זאת כדי לגשת למידע חשוב או לעבוד על פרויקטים שונים. אחרים יורדים דפים כדי לבצע בדיקות ולאבחן בעיות עם אתרים.

## כיצד לעשות

להלן דוגמאות לקוד ולפלט עם בלוקי קוד "```Rust ... ```":

```Rust
use reqwest::blocking::Client;

fn main() {
    // קוד לבצע חיבור לאתר עם הספרייה reqwest
    let client = Client::new();
    let response = client.get("https://www.example.com").send().unwrap();
    
    // קוד להדפסת קוד תגובת האתר
    println!("קוד תגובה: {}", response.status());
    
    // קוד להדפסת תוכן התגובה
    let body = response.text().unwrap();
    println!("תוכן תגובה: {}", body);
}
```

פלט:

```
קוד תגובה: 200 OK
תוכן תגובה: <!doctype html>
<html>
<head>
  <title>דף דוגמה</title>
  <meta charset="utf-8">
</head>
<body>
  <h1>שלום עולם!</h1>
</body>
</html>
```

## Deep Dive

השיטות להורדת דפים אינטרנטיים יכולות להיות מגוונות ומורכבות יותר בהתאם לצרכים המיוחדים של כל משתמש. לדוגמה, ניתן להשתמש בספריות מתקדמות נוספות כגון Serde כדי לטפל בתצורה תקינה של תגובות JSON מהאתרים. ניתן גם להשתמש בתכנית לומדות עמוקה יותר על הפונקציות והממשקים של מנהלי התקשורת HTTP כדי לבנות יישומי אינטרנט עוצמתיים יותר.

## ראה גם

- [מדריך מתחילים לשפת תכנות Rust](https://www.codecademy.com/learn/learn-rust)
- [ניידות וביצועים מתקדמים עם Rust](https://www.youtube.com/watch?v=u1A0FZzp54s)
- [תיעוד הספרייה reqwest של Rust](https://docs.rs/reqwest/0.11.3/reqwest/index.html)