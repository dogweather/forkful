---
date: 2024-01-26 04:18:44.409057-07:00
description: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E9\u05D5\u05E8\u05EA \u05E4\
  \u05E7\u05D5\u05D3\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\
  \u05D9\u05EA, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\
  \u05D4-\u05D4\u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4 (REPL), \u05DE\
  \u05D0\u05E4\u05E9\u05E8 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E7\u05D5\u05D3 \u05D1\
  \u05D0\u05D5\u05E4\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\
  \u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05E7\u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3 \u05E9\u05DC Swift\u2026"
lastmod: '2024-03-13T22:44:39.908539-06:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E9\u05D5\u05E8\u05EA \u05E4\u05E7\
  \u05D5\u05D3\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\
  \u05EA, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4\
  -\u05D4\u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4 (REPL), \u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E7\u05D5\u05D3 \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E7\
  \u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3 \u05E9\u05DC Swift\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש בשורת פקודה אינטראקטיבית, או לולאת קריאה-הערכה-הדפסה (REPL), מאפשר לכתוב קוד באופן אינטראקטיבי. מתכנתים משתמשים בזה כדי לבדוק קטעי קוד של Swift במהירות, לאבחן באגים, או ללמוד את השפה.

## איך לעשות זאת:
הפעל REPL על ידי פתיחת טרמינל והרצת `swift`. הקלד קוד ישירות ולחץ Enter כדי להריץ אותו. הנה טעימה:

```Swift
1> let greeting = "Hello, REPL!"
greeting: String = "Hello, REPL!"
2> print(greeting)
Hello, REPL!
```

יציאה עם `:quit` או `Control-D`.

## צלילה עמוקה
שורשי REPL חוזרים הרבה אחורה לפרשני Lisp בשנות ה-60. REPL של Swift יושבת על גבי LLVM, מסגרת קומפיילר חזקה, המציעה יותר מפשוט פרשנות—היא כלי מלא עם השלמה אוטומטית, איתור באגים, ועוד. REPL מעולה ללמידה או פרוטוטייפינג, אבל היא לא סביבת פיתוח עצמאית. ישנם אנשים המעדיפים להשתמש בכיכרות משחק ב-Xcode לגישה גרפית ובהתבסס על קבצים, בעוד אחרים ממשיכים לערוך ולהריץ סקריפטים באופן מסורתי.

מאחורי הקלעים, REPL של Swift מקמפלת דינמית את הקוד לשפת המכונה ומבצעת אותו, זו הסיבה שהיא יחסית מהירה. היא יכולה גם לגשת לכל מודולי Swift שנקמפלו, או אפילו לספריות C, מה שהופך אותה לדי חזקה. שימו לב, עם זאת, שלא הכל פועל בצורה מושלמת ב-REPL; כמה תכונות של Swift, במיוחד אלו שדורשות הגדרות פרויקט מורכבות או קבצי storyboard, לא יעבדו כאן.

## ראה גם
- [Swift.org - קידום מהיר](https://www.swift.org/getting-started/#using-the-repl)
- היכרות עם כיכרות המשחק של אפל ב-Xcode  [Introduction to Xcode Playgrounds](https://developer.apple.com/videos/play/wwdc2014/408/)
- [פרויקט LLVM](https://llvm.org/)
