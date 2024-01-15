---
title:                "כתיבת קובץ טקסט"
html_title:           "Elm: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מדוע: מקסימום שני משפטים שמסבירים *למה* מישהו יבחר לכתוב קובץ טקסט.

שיטת העבודה הנייחת של ELM מאפשרת למפתחים ליצור קשרים קלים וברורים בין נתונים ורשתות להגדרות, מאפשרת יצירה ש לאחר התבוננות ארוכה, ומובטחת תחילה לחלוטין, וכוללת וכישור אנושי, וכוללת כישור אוטומטי ובעצמו.

## כיצד לכתוב קובץ טקסט ב-ELM

כדי ליצור קובץ טקסט ב-ELM, ניתן להשתמש בפונקציות פשוטות כגון "text" ו"toText". לדוגמא:

```Elm
import Text

filePath = "myFile.txt"
text = "Hello world!"

main =
  Text.toText text |> Text.text filePath
```

פקודת "toText" ממירה את הטקסט שהוגדר לפורמט טקסט בכדי שיהיה אפשרי להכניסו לקובץ. משתמשים בפונקציה "text" כדי ליצור את הקובץ עם הטקסט המבוקש בנתיב שהוגדר.

בנוסף, ניתן להשתמש בפקודת "appendText" על מנת להוסיף טקסט לקובץ קיים. לדוגמא:

```Elm
import Text

filePath = "myFile.txt"
text = "Hello world!"

main =
  Text.toText text |> Text.appendText filePath
```

## מעמקים

כאשר מבינים כיצד לכתוב קבצי טקסט ב-ELM, ניתן להשתמש בפונקציות נוספות כדי להתאים ולעצב את הטקסט המבוקש. לדוגמא, ניתן להשתמש בפונקציות כמו "leftAligned", "centered" ו"rightAligned" כדי ליישם תסמונת לקובץ הטקסט. ניתן גם להשתמש בפונקציות כמו "bold" ו"italics" על מנת לשנות את עיצוב הטקסט. ישנם גם פונקציות נוספות כמו "line" ו-"paragraph" שאפשר להשתמש בהן כד