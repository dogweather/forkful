---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases:
- /he/swift/using-an-interactive-shell-repl/
date:                  2024-01-26T04:18:44.409057-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-an-interactive-shell-repl.md"
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
