---
title:                "Elm: חישוב תאריך בעתיד או בעבר"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

למה לעסוק בחישוב תאריך בעתיד או בעבר? החישוב של תאריך יכול לעזור לנו לתכנן את הופעה שלנו או לבדוק את הזמן שאנחנו ניצל את התאריך הנכון. זה יכול להיות שימושי במיוחד לכאלו שעובדים עם לוח שנה או מתכנני אירועים.

## כיצד

ראשית, נצטרך לייבא את המודול `Date` מתוך חבילת Elm בעזרת הפקודה הבאה:

```Elm
import Date
```

לאחר מכן, נוכל להשתמש בפונקציה `add` שבתוך המודול כדי להוסיף או להחסיר ימים, שבועות, חודשים או שנים מתאריך מסוים. לדוגמה, נרצה לחשב את התאריך בעוד 7 ימים מהתאריך הנוכחי. הנה כיצד נוכל לעשות זאת:

```Elm
Date.add Date.days 7 Date.today
```

יהיה לנו התאריך המעודכן בתאריך העתידי שנמצא 7 ימים מאתה נמצא בתאריך התחלתי.

אנחנו יכולים גם להשתמש בפונקציה `subtract` לצורך חילוץ תאריך בעבר:

```Elm
Date.subtract Date.weeks 2 Date.today
```

זה יחזיר לנו את התאריך ששני שבועות מקדימה מתאריך הנוכחי.

## חפירה עמוקה

המודול `Date` מכיל גם פונקציות נוספות לעיבוד תאריכים, כמו `fromString` להמרת תאריך מתחריף לטקסט ו`toString` להמרת תאריך לתחריף. בנוסף, ניתן להשתמש בפונקציות נוספות כמו `toTime` ו- `fromTime` לעיבוד תאריכים בפורמט תאריך קוננוני.

## ראה גם

- [Documentation for Elm's `Date` module in English](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [חבילת Elm לגיבוב ועיבוד תאריכים](https://package.elm-lang.org/packages/elm/time/latest/)
- [מדריך ללמידת Elm בעברית](https://github.com/jamiedixon/learn-elm-in-hebrew)