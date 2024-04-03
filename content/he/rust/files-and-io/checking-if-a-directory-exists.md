---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:09.779340-07:00
description: "\u05D1\u05E4\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D5\u05DB\u05E0\u05D4\
  , \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E0\
  \u05D3\u05E8\u05E9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA\
  \ \u05DC\u05DE\u05E0\u05D5\u05E2 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D1\u05E2\
  \u05EA \u05E0\u05D9\u05E1\u05D9\u05D5\u05DF \u05DC\u05D2\u05E9\u05EA, \u05DC\u05E7\
  \u05E8\u05D5\u05D0, \u05D0\u05D5 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E7\u05D1\u05E6\
  \u05D9\u05DD. \u05E8\u05D0\u05E1\u05D8, \u05D1\u05EA\u05D5\u05E8 \u05E9\u05E4\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05E2\u05E8\u05DB\u05EA, \u05DE\u05E1\u05E4\
  \u05E7\u05EA \u05E9\u05D9\u05D8\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.011046-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05E4\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D5\u05DB\u05E0\u05D4, \u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E0\u05D3\
  \u05E8\u05E9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\
  \u05DE\u05E0\u05D5\u05E2 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D1\u05E2\u05EA\
  \ \u05E0\u05D9\u05E1\u05D9\u05D5\u05DF \u05DC\u05D2\u05E9\u05EA, \u05DC\u05E7\u05E8\
  \u05D5\u05D0, \u05D0\u05D5 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E7\u05D1\u05E6\u05D9\
  \u05DD."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## איך לעשות:
ספריית התקן של ראסט (`std`) כוללת פונקציונליות לבדיקת קיום של ספרייה דרך המודולים `std::path::Path` ו-`std::fs`. הנה דוגמה פשוטה באמצעות הגישה הסטנדרטית של ראסט:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("הספרייה קיימת.");
    } else {
        println!("הספרייה לא קיימת.");
    }
}
```

פלט לדוגמה, בהנחה שהספרייה קיימת:
```
הספרייה קיימת.
```

לצורכי תרחישים מורכבים יותר או תכונות מתקדמות (כמו פעולות מערכת קבצים אסינכרוניות), ייתכן ותשקול להשתמש בספרייה צד שלישי כמו `tokio` עם המודול `fs` האסינכרוני שלה, במיוחד אם אתה עובד בסביבת ריצה אסינכרונית. הנה איך ניתן להשיג את אותו הדבר עם `tokio`:

ראשית, הוסף את `tokio` ל-`Cargo.toml` שלך:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

לאחר מכן, השתמש ב-`tokio::fs` כדי לבדוק אם ספרייה קיימת באופן אסינכרוני:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("הספרייה קיימת.");
            } else {
                println!("הנתיב קיים אך אינו ספרייה.");
            }
        },
        Err(_) => println!("הספרייה לא קיימת."),
    }
}
```

פלט לדוגמה, בהנחה שהספרייה אינה קיימת:
```
הספרייה לא קיימת.
```

הדוגמאות הללו מדגישות כיצד ראסט והאקוסיסטם שלה מציעים גישות סינכרוניות ואסינכרוניות לבדיקת קיום ספריות, שמתאימות למגוון רחב של צרכים בפיתוח תוכנה.
