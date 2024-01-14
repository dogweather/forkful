---
title:                "Rust: לשנות את כותרת המחרוזת לאותיות רישיות"
simple_title:         "לשנות את כותרת המחרוזת לאותיות רישיות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מדוע

למה לאדם ישאגדו בלכת עם ראסט לדרך?

ככל הידוע, ראסט הוא שפת תכנות אפיונית מתקדמת ומספקת כלים עצמאיים ואפקטיביים לכתיבת קוד קל לקריאה ולתחזוקה. אחת התכונות המעניינות של ראסט היא אפשרות לנתח ולעבד טקסט בקלות. תהליך מה של עיבוד טקסט, כמו למשל כתיבה או עיצוב את הנתונים, הוא נפוץ הן בתעשייה והן בקוד פתוח. אחרי כל, מי כבר לא רוצה להציג טקסט בצורה שקופה ונגישה?

## איך לעשות זאת

כדי לנתח את הטקסט ולכתוב אותו באופן מסודר ונוח לקריאה, ניתן לעבוד עם הפונקציה capitalize של ראסט. בדוגמה הבאה, נכניס טקסט ונייצג אותו באופן מגוון על ידי עיבוד הטקסט בכמה דרכים שונות:

```Rust
let text = "hello world!";
let upper_case_text = text.to_uppercase();
let lower_case_text = text.to_lowercase();
let capitalized_text = text.capitalize();
println!("Original text: {}", text);
println!("Uppercase text: {}", upper_case_text);
println!("Lowercase text: {}", lower_case_text);
println!("Capitalized text: {}", capitalized_text);
```

תוצאה:

```
Original text: hello world!
Uppercase text: HELLO WORLD!
Lowercase text: hello world!
Capitalized text: Hello world!
```

ניתן לראות שקיבלנו תוצאות שונות בהתאם לפונקציה שהשתמשנו בה. האפשרויות המוכרות של הפונקציה capitalize הן לכתוב את המילה הראשונה ברשימה באות גדולה ואת שאר המילים באותיות קטנות. אבל אם אתה רוצה לנדבק במתכון שהוציאו מהפורום שלפני שנים, אז אתה יכול פשוט להפעיל to_capitalized_word במקום capitalized.

בנוסף, ניתן להשתמש גם בפונקציה capitalize_utf8 אם אתה רוצה לעבוד עם טק