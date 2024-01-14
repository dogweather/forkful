---
title:                "Elm: יצירת קובץ זמני"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

בתכנות, כמעט בכל פרויקט, תמיד תקיים צורך ליצור קבצים זמניים. למרבה המזל, בְּאֶלְמוֹ, ניתן להתמודד עם הצורך הזה בקלות רבה. למרבה המעט, לכם יש כתבי SQL ו CSV מכונים {\`{\`{Elm}} קיבצים זמניים למטרת בדיקת קוד.

## איך לעשות

```Elm
import File
import Process

createTempFile : String -> Cmd msg
createTempFile content =
  let
      fileName = "temp.txt" -- שם הקובץ הזמני
      filePath = "./" ++ fileName -- הנתיב של הקובץ בתוך הספרייה הנוכחית של המשתמש
      tempFile = File.write filePath content -- פקודת טיפול בקובץ זמני
      process = Process.sleep 500 -- פקודת עצירת פרוצס הבררת בש
  in
      Cmd.batch [tempFile, process] -- ערכת פקודות זמניות שתפעיל את כל הפקודות בבת אחת

```

ErrorMessage למה לבדוק אם הפעולה נבעת מהצליחה, תאפשר לנו לקבל למידע מועיל יותר כאשר המערכת כוללת קובץ זמני.

## חקירה עמוקה

יצירת קבצים זמניים מוכרת כדרך לדעת האם הקוד שלנו עובד כראוי ומה עומד מאחורי התקינה שכתבנו. שנהיה חפים עתור מהקבצים הזמניים שיצרנו, אחת התוכניות שיש כדאי להריץ היא לקרוא ל-npos. פתח לקבלב להגיד-N.B., תוכל ללמוד היטב בעזרת הערוץ הזה

## ראה גם

- ערוץ העזרה של אלם: https://guide.elm-lang.org/
- דף הורדה של אלם: https://elm-lang.org/assets/downloads/