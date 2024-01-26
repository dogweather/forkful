---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:54:28.693309-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט היא פשוט לגשת לנתונים מאוחסנים בקובץ. תוכניתנים עושים זאת כדי לטעון, לעבד או להציג מידע.

## איך לעשות:
```gleam
import gleam/io
import gleam/result.{Result, Ok, Error}

pub fn read_file(path: String) -> Result(String, String) {
  try file = io.open(path)
  let content = io.read(file)
  io.close(file)
  content
}

pub fn main() {
  case read_file("hello.txt") {
    Ok(content) -> io.println(content)
    Error(err) -> io.println("An error occurred: " ++ err)
  }
}
```
פלט לדוגמה:
```
Hello, Gleam!
```

## צלילה לעומק
בעבר, קריאת קובץ היתה מסובכת יותר. עם הזמן, שפות תכנות פיתחו ספריות שמפשטות את התהליך. ב-Gleam, קריאת קבצים מבוססת על פונקציות מידע שמשמשות לגישה למערכת הקבצים של המערכת המובילה. קיימות שיטות אחרות, כמו זרימת קלט/פלט (streams), אבל ללמוד איך לעבוד עם קובץ בסיסי הוא נקודת פתיחה טובה.

## ראה גם:
- [Erlang's file module, for more advanced file operations](http://erlang.org/doc/man/file.html)
- [Elixir's File module, for those also using Elixir in their stack](https://hexdocs.pm/elixir/File.html)
