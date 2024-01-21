---
title:                "התחלת פרויקט חדש"
date:                  2024-01-20T18:04:02.702986-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
כשאנחנו פותחים פרויקט חדש ב-Gleam, אנחנו בעצם יוצרים התחלה נקייה עבור רעיון או פתרון תוכנה שלנו. מתכנתים עושים את זה כדי לחשוב על בעיות בתור מבנים מופשטים, ולבנות פתרון יעיל ונקי מאפס.

## איך לעשות:
```Gleam
// התקנת Gleam על מערכת ההפעלה שלך
$ rebar3 new gleam my_cool_project
$ cd my_cool_project

// כתיבת הקוד שלך. נניח, יצירת פונקציה פשוטה בקובץ main.gleam
pub fn hello_world() -> String {
  "Hello, Gleam!"
}

// הרצת הפרויקט וראיית הפלט
$ rebar3 shell
1> my_cool_project:hello_world().
"Hello, Gleam!"
```

## צלילה עמוקה
Gleam נולד כשפת תוכנה פונקציונלית עם טייפינג חזק וסטטי, מושפעת מ-Erlang ו-Elm. כשאתה פותח פרויקט חדש ב-Gleam, אתה יכול להיעזר בכלים כמו `rebar3`, הבנוי לעבודה עם מערכת ה-Erlang/OTP. אלטרנטיבות? יש Elixir ו-LFE (Lisp Flavoured Erlang), אבל Gleam מבטיחה ביצועים גבוהים עם טייפינג חזק. ביצועי פרויקט מתחילים עם הבנה של תיקיות הפרויקט והתאמה לתוכנית העבודה שלך.

## ראה גם
- [מסמך התחלה מהירה של Gleam](https://gleam.run/book/getting-started/)
- [מאגר גיטהאב של גלים](https://github.com/gleam-lang/gleam)