---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/rust/checking-if-a-directory-exists/
date:                  2024-02-03T19:09:09.779340-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בפיתוח תוכנה, לעיתים קרובות נדרש לבדוק אם ספרייה קיימת על מנת למנוע שגיאות בעת ניסיון לגשת, לקרוא, או לכתוב קבצים. ראסט, בתור שפת תכנות מערכת, מספקת שיטות עמידות לביצוע משימה זו, ובכך מבטיחה שהתוכנית שלך תוכל לטפל בקבצים ובספריות באופן בטוח ויעיל.

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
