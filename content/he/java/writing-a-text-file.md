---
title:                "לכתיבת קובץ טקסט"
html_title:           "Java: לכתיבת קובץ טקסט"
simple_title:         "לכתיבת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה
כתיבת קובץ טקסט היא פעולה חשובה ומשתלמת למתכנתי ג'אווה. היא מאפשרת ליצור קובץ שיכיל מידע טקסטואלי ולשמור אותו כחלק מהתוכנה, מה שיכול לשמש ככלי מאוד שימושי לארגון וניהול מידע.

## איך לעשות זאת
כדי לכתוב קובץ טקסט בג'אווה, ישנם מספר שלבים פשוטים שצריך לעשות:

```Java
// צור אובייקט של FileWriter
FileWriter fw = new FileWriter("myFile.txt");

// כתוב מלל לקובץ
fw.write("זהו טקסט שיהיה בקובץ הטקסט החדש שלי.");

// סגור את הערך
fw.close();
```

הקוד הזה יצור קובץ עם השם "myFile.txt" ויכתוב בתוכו את המלל שנתתי לו. לאחר מכן, ייסגר הקובץ כדי שהמידע יישמר בצורה תקינה.

## Deep Dive
בנוסף לכתיבת טקסט לקובץ, תוכל גם להשתמש במגוון כלים נוספים כדי להתאים את הקובץ לצרכים המדוייקים שלך. לדוגמה, תוכל להשתמש במחלקה PrintWriter כדי להכניס נתונים מסוגים שונים לקובץ, כגון מספרים או מערכים. תוכל גם להשתמש במחלקות Scanner ו- BufferedReader כדי לקרוא מידע מהקובץ ולעבוד איתו בצורה מתקדמת.

## ראה גם
+ [מסמך רשמי של שפת תכנות ג'אווה](https://docs.oracle.com/en/java/javase/index.html)
+ [התחברות לקבצים וכתיבה להם באמצעות ג'אווה](https://www.baeldung.com/java-write-to-file)
+ [דוגמאות לכתיבת קבצים טקסט בג'אווה](https://www.journaldev.com/878/java-write-to-file)