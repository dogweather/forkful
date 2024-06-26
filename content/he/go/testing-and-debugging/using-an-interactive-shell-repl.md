---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:02.145734-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05DE\u05E8\
  \u05D5\u05EA \u05E9\u05D1-GO \u05D0\u05D9\u05DF \u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA \u05DE\u05D5\u05D1\
  \u05E0\u05D9\u05EA, \u05D4\u05E7\u05D4\u05D9\u05DC\u05D4 \u05D9\u05E6\u05E8\u05D4\
  \ \u05DB\u05DC\u05D9\u05DD \u05DB\u05DE\u05D5 `gore` \u05DB\u05D3\u05D9 \u05DC\u05DE\
  \u05DC\u05D0 \u05D0\u05EA \u05D4\u05D7\u05E1\u05E8. \u05E8\u05D0\u05E9\u05D9\u05EA\
  , \u05D4\u05EA\u05E7\u05DF \u05D0\u05EA `gore` \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\
  \u05E8\u05E6\u05EA."
lastmod: '2024-03-13T22:44:38.493098-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05DE\u05E8\u05D5\u05EA \u05E9\u05D1-GO \u05D0\u05D9\u05DF \u05DE\
  \u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\
  \u05D9\u05EA \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA, \u05D4\u05E7\u05D4\u05D9\u05DC\
  \u05D4 \u05D9\u05E6\u05E8\u05D4 \u05DB\u05DC\u05D9\u05DD \u05DB\u05DE\u05D5 `gore`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05DE\u05DC\u05D0 \u05D0\u05EA \u05D4\u05D7\u05E1\u05E8\
  ."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך לעשות:
למרות שב-GO אין מעטפת אינטראקטיבית מובנית, הקהילה יצרה כלים כמו `gore` כדי למלא את החסר. ראשית, התקן את `gore` על ידי הרצת:

```
$ go get -u github.com/motemen/gore
```

לאחר ההתקנה, הפעל את `gore` על ידי הקלדת `gore` בטרמינל שלך:

```
$ gore
```

כעת תראה שורת פקודה מוכנה לקבל פקודות ב-GO. בואו ננסה דוגמא פשוטה:

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

התוצאה שתראה תהיה:

```
Hello, Go REPL!
```

הגדרת משתנים ופונקציות עובדת כצפוי. אתה יכול להגדיר פונקציה:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Area of circle with radius 4:", areaCircle(4))
```

ולקבל את התוצאה מיד:

```
Area of circle with radius 4: 50.26548245743669
```

## צלילה עמוקה:
הרעיון של REPL עתיק, חוזר למחשבי Lisp של שנות ה-60, ומספק חוויה אינטראקטיבית של תכנות. בניגוד לשפות כמו Python או JavaScript, GO נוצרה ללא REPL, עם דגש על בינארים מהודרים לביצועים ופשטות. זה משקף את פילוסופיית הפשטות של GO ואת עיצובה לתוכנה נתפסת ונתמכת.

עם זאת, כלים כמו `gore` או `goplay` מציגים את המרץ של קהילת GO בגישור על הפער הזה. כלים אלה מנתחים קוד GO דינאמית ומשתמשים בחבילה `go/eval` או מנגנונים דומים לביצועו בזמן אמת, אם כי עם מגבלות מסוימות בהשוואה לסביבת REPL מקורית. המגבלות האלו נובעות ממערכת הטיפוסים ומודל ההידור של GO, שיכולים להפוך את ההערכה בזמן אמת לאתגר.

למרות שסביבות REPL מועילות במיוחד לחינוך ובדיקות מהירות, אקוסיסטם של GO בדרך כלל נוטה לכיוון תהליכי הידור-והרצה המסורתיים לרוב משימות הפיתוח. סביבות פיתוח ועורכים עם תמיכה ב-GO, כמו Visual Studio Code או GoLand, מציעים כלים משולבים לבדיקה ולניפוי באגים שמקלים במידה רבה על הצורך ב-REPL לפיתוח מקצועי.

לתכנות חקרנית, יצירת טייפים או למידה, עם זאת, REPLs כמו `gore` מציעים חלופה יקרת ערך, מאפשרים למתכנתים הרגילים ל-REPLs בשפות אחרות ליהנות מחוויה דומה ב-GO.
