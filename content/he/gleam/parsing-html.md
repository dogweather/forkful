---
title:                "Gleam: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/parsing-html.md"
---

{{< edit_this_page >}}

Shalom,

## מדוע

מאחר ואיננו יכולים לשנות את מבנה האתרים באינטרנט, עיבוד ומיון (Parsing) של קוד HTML הינו מהותי בכדי לקרוא ולהבין מידע מרובה בקודים מורכבים.

## כיצד לבצע

```Gleam
let parser = Parser.from_string("<html><body><h1>Hello, world!</h1></body></html>")

try case parser of
| Ok(nodes) ->
  io.println(nodes)
| Error(error) ->
  // handle parse error
```

כאן אנו משתמשים בפונקציית `Parser.from_string()` כדי ליצור מפענח חדש. מאחר והקוד שראינו הינו תקין, הפענח יחזיר כתוצאה רשימת תגיות מקודדות את המידע שלנו. בדוגמה שלנו, הפלט יחזיר `["html", "body", "h1", "Hello, world!"]` כמו כן, במידה והקוד לא יהיה תקין, נתקל בשגיאת עיבוד (Parsing) ונוכל להתמודד עם השגיאה באמצעות הבלוק `case` המאפשר לנו לטפל בכל מצב אפשרי.

## חפירה מעמוק

עיבוד (Parsing) הינו פעולה בסיסית במיני-שפת Gleam. מאחר והיא מאפשרת לנו לקרוא פונקציות ויצירת מהפכות בעולם האינטרנט, חשוב להשקיע בלימוד עמוק של מתודות עיבוד המידע. מרבית הפעמים אנו נתקלים בקודים מורכבים עם הרבה תגיות ומידע רב, ועלינו לדעת איך לעבוד ולחלץ את המידע הרלוונטי בכדי להשתמש בו במימושים ויצירת אפליקציות.

## ראו גם

- [Gleam פוקים](https://gleam.run/)
- [רכיב HTML-5 (גליים)](https://github.com/gleam-lang/html5)
- [למד גליים בפעילות Gleam School](https://luminiferous.solutions/gleam-school/)