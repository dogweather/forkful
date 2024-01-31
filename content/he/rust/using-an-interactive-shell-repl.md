---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:18:55.572082-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
קונסולה אינטרקטיבית של Rust, או REPL (Read-Eval-Print Loop), מאפשרת לך להריץ קוד Rust בזמן אמת ולראות תוצאות מיידיות, מושלם לניסויים או ללמידה. מתכנתים משתמשים בה כדי לבדוק קטעי קוד, לאבחן באגים, או פשוט לשחק עם תכונות השפה מבלי להתעסק בנטל הקומפילציה של פרוייקט מלא.

## איך לעשות זאת:
כרגע, ל-Rust אין REPL רשמי שמגיע עם השפה. אתה יכול להשתמש בכלים של צד שלישי כמו `evcxr_repl`. התקן אותו באמצעות Cargo:

```sh
cargo install evcxr_repl
```

לאחר מכן, הפעל את ה-REPL:

```sh
evcxr
```

בתוכו, נסה קצת קוד Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

הפלט אמור להיות:

```
5 + 3 = 8
```

## צלילה עמוקה
האתוס של Rust ממוקד סביב בטיחות וביצועים, אשר בדרך כלל משויכים לשפות שמקומפלות מראש, פחות עם שפות מפורשות, ידידותיות ל-REPL. באופן היסטורי, שפות כמו Python או Ruby שמו דגש על בעלות REPL למשוב מיידי, אך לא תוכננו עם מטרות ברמת המערכת בראש.

למרות חוסרה של REPL רשמית ב-Rust, כמה אלטרנטיבות כמו `evcxr_repl` צמחו. הפרויקטים הללו אינם רק "משחקים" עם Rust כדי להפוך אותה ל-REPL; הם קוראים בחכמה את מעגל הקומפילציה והריצה של השפה לסשן אינטרקטיבי. ה-REPL מקומפל את הקוד ברקע ומריץ את הבינארי, תוך תיעוד הפלט. בדרך זו, היא שומרת על יתרונות הביצועים של Rust תוך כדי שהיא מעניקה את חווית האינטראקציה.

יש דיון מתמשך בקהילת Rust לגבי תמיכה רשמית ב-REPL, ועם כל איטרציה של השפה, אנו רואים רמת מסופקות כלים שעשויה להוביל בסופו של דבר לפתרון מובנה.

## ראה גם
למידע נוסף וכלים אחרים:
- מאגר GitHub של Evcxr REPL: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, דרך מקוונת לבדוק קוד Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- דיון בשפת Rust על תכונת REPL: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
