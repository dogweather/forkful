---
title:                "מילון למןחשבת תכניתווץ ניתוח גבולות של מחרוזת"
html_title:           "Rust: מילון למןחשבת תכניתווץ ניתוח גבולות של מחרוזת"
simple_title:         "מילון למןחשבת תכניתווץ ניתוח גבולות של מחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
Interpolating היא תהליך בתכנות שבו מחרוזת מיוחסת מוחלפת עם ערכים אחרים במהלך הקוד. תהליך זה ניתן להשתמש בו על מנת ליצור מחרוזות דינמיות ולהכניס מידע משתנה לתוך המחרוזת. מתכנתים משתמשים בתהליך זה כדי ליצור תוכניות איכותיות יותר ולשפר את נסיגת הקוד.

## איך לעשות?
דוגמאות קוד ותוצאה מתוך הקוד ```Rust ...``` code blocks.

דוגמאות של תהליכי מחרוזת מיוחסות:

- דוגמא פשוטה עם ערך קבוע: 
```
let name = "Amir";
let greeting = format!("Hello, {}!", name);
```
תוצאה:
```
Hello, Amir!
```

- דוגמא עם שתי משתנים:
```
let name = "Amir";
let country = "Israel";
let greeting = format!("Hello, {}! Welcome to {}!", name, country);
```
תוצאה:
```
Hello, Amir! Welcome to Israel!
```

- דוגמא עם משתנים מספריים:
```
let age = 28;
let years_in_company = 5;
let announcement = format!("I am {} years old and have been working at this company for {} years.", age, years_in_company);
```
תוצאה:
```
I am 28 years old and have been working at this company for 5 years.
```

## חפירה מעמיקה
בעבר, מחרוזות מיוחסות נכתבו כעת בעמודות קוד קשיחות וקשות לניהול. אך עם מודעות המאפשרת שגרת זה פתרון פשוט לבעיה זו. יחד עם זאת, ישנם גם פתרונות אחרים למחרוזות דינמיות כמו תבניות שימוש בתוך מחרוזות תוך שימוש בתוספות כאשר משתנים מיוחסים בכפרות, לכן חשוב לבחון את התכנים והמטרות של התוכנית לפני בחירת תהליך מתאים.

## ראו גם
למידע נוסף על תהליכי מחרוזות מיוחסות ומשתנים, ניתן לבדוק את התיעוד של Rust ולעיין בדוגמאות של קוד. ניתן גם למצוא טיפים והנחיות באתר הרשמי של Rust ובמקורות פתוחים אחרים באינטרנט.