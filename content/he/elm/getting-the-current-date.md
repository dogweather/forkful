---
title:                "Elm: קבלת תאריך נוכחי"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מדוע

קבלת תאריך נוכחי היא פעולה חשובה בתכנות של כל שפת תכנות. תאריך נוכחי מאפשר לנו להציג בצורה נוחה נתונים כגון תאריך ושעה, או לבדוק את המספר הנוכחי של היום בחודש. כללי, כל יישום המשתמש בזמן חייב לצורך דבר את תאריך היום הנוכחי.

## איך לעשות

תאריך נוכחי ניתן לקבל באמצעות פונקציית `Date.now` בשפת תכנות Elm. בלוק הקוד הבא מציג דוגמה לאיך להציג את התאריך הנוכחי:

```elm
Date.now
|> Date.toYearMonthDay
|> toString
```

בתוצאה המשתנה ניתן לראות את התאריך הנוכחי בפורמט שנבחר. הנה דוגמה לתוצאה שאולי נרצה לקבל:

`"1/10 האפריל של שנת 2021"`

## לחקור

למרבה המזל, קבלת תאריך נוכחי לא נוראי כמו שהעולם נותן לנו להאמין. היישום שלה לפעולות זמן מחשב יכול להיות מורכב ומיוחד לתנאי השימוש שלך. אם אתה רוצה לחשוב עוד יותר על הנושא הזה, הנה כמה משאבים נהדרים לקריאה:

- [תיעוד: פונקציית Date.now של אלם](https://package.elm-lang.org/packages/elm/time/latest/Time#now)
- [דוגמה לכתיבת יישום כדי להציג את התאריך הנוכחי](https://github.com/elm/projects/tree/master/elm-time/examples/iso8601)
- [מאמר מעניין על תוכניות זמן מניפולציות בתכנות באמצעות אלם](https://dev.to/ilonacodes/time-travel-manipulating-date-and-time-with-elmectron-33hb)

## ראה גם

- [רדיקס כתבה הפועל על נושא דומה](https://redux.js.org/recipes/using-external-apis)
- [תחזוקת אלם דוקומנטציה של תמיכה בזמן זיכרון](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode-Extra#parseFlexibleDate)