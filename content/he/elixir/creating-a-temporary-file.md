---
title:                "יצירת קובץ זמני"
aliases:
- he/elixir/creating-a-temporary-file.md
date:                  2024-01-20T17:41:05.542340-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה שמאפשרת לעבוד עם נתונים באופן נטול סיכון במערכת הקבצים, ואז למחוק אותם. תכניתנים עושים את זה כאשר הם צריכים לשמור נתונים באופן זמני לצורך עיבוד או כאשר רוצים למנוע זיהום מחיצות קבצים עיקריות.

## איך לעשות:
ב-Elixir, ניתן ליצור קבצים זמניים באמצעות מודולים כמו `File` ו`Path`. הנה דוגמה פשוטה:

```elixir
{:ok, file_path} = File.open("tmp/my_temp_file", [:write, :exclusive, :tempfile])
File.write!(file_path, "זהו טקסט זמני")
File.read!(file_path)
File.rm!(file_path)
```

על ידי שימוש באופציה `:tempfile`, מובטח שהקובץ יהיה זמני. הדוגמה לעיל פותחת קובץ, כותבת לו, קוראת ממנו ולבסוף מוחקת אותו.

## צלילה עמוקה
השימוש בקבצים זמניים אינו חידוש - הוא נמצא בשימוש מאז ימי המחשב הראשונים, כאשר המשאבים היו מוגבלים וניהול מחיצות קבצים היה קריטי יותר. ב-Elixir, מימוש קבצים זמניים מתבצע בצורה מאוד מודרנית וטבעית לפילוסופיה של השפה, אשר מעודדת פעולות בטוחות וצרכניות של משאבים.

השימוש באופציה `:exclusive` מבטיח שהקובץ יהיה בלעדי לתהליך שפתח אותו, מה שמגביר את הבטיחות ומונע התנגשויות. יתר על כן, קבצים זמניים נמחקים לרוב באופן אוטומטי בסיום התוכנית או התהליך שיצר אותם, למרות שהמחיקה הידנית שבדוגמה היא תמיד טובה לווידוא שהכל נעלם כשאתה רוצה.

## ראו גם
- [Elixir File module](https://hexdocs.pm/elixir/File.html)
- [Elixir Path module](https://hexdocs.pm/elixir/Path.html)
- [Elixir School: Working with files](https://elixirschool.com/en/lessons/basics/collections/#files)

הקישורים הללו ייקחו אותך למסמכים הרשמיים של Elixir על עבודה עם קבצים ונתיבים, ולאתר Elixir School, שבו תמצאו מדריכים בסיסיים ומתקדמים שמתאימים ללמידה בקצב שלך.
