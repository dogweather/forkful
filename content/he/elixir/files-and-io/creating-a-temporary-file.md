---
date: 2024-01-20 17:41:05.542340-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E0\u05D8\u05D5\u05DC\
  \ \u05E1\u05D9\u05DB\u05D5\u05DF \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\
  \u05D1\u05E6\u05D9\u05DD, \u05D5\u05D0\u05D6 \u05DC\u05DE\u05D7\u05D5\u05E7 \u05D0\
  \u05D5\u05EA\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D0\u05E9\u05E8 \u05D4\u05DD\
  \ \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF\u2026"
lastmod: '2024-03-13T22:44:38.805924-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E0\u05D8\u05D5\u05DC \u05E1\
  \u05D9\u05DB\u05D5\u05DF \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\
  \u05E6\u05D9\u05DD, \u05D5\u05D0\u05D6 \u05DC\u05DE\u05D7\u05D5\u05E7 \u05D0\u05D5\
  \u05EA\u05DD."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

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
