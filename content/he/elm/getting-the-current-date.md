---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
לקבלת התאריך הנוכחי באמצעות הקוד מאפשר לנו לעקובאחרי זמן ריצת התוכנית, ליצור תגיות תאריך לאירועים, ולהתמודד עם זמן ממשי. זה חיוני למגוון של יישומים, כולל יישומים של שרת וממשק משתמש.

## איך לעשות: 

קוד Elm צפוי לשליפת התאריך הנוכחי הוא בעצם פונקציה שמחזירה `Task`, אשר מציין פעולה שצריכה להתבצע בזמן ריצה:

```Elm
import Time

getTime : Task x Time.Posix
getTime =
    Time.now
```
בחלק זה של הקוד, פונקצית ה`now` ממודול `Time` נקראת להחזיר את השנייה הנוכחית מאז אפס הזמן, מאגר POSIX.

## צלילה עמוקה: 

באופן היסטורי, אותה טכניקה של שליפת התאריך והשעה משמשת מתחילת המחשב. השפה של Elm רק מבצעת אותה בצורה הפשוטה והמפורשת ביותר.

למרות שהגישה הזו היא המקובלת ביותר על ידי הקהל של Elm, דרכים אחרות יכולות לעזור לטפל בתאריך ובזמן בכדי להתאים יותר לצורכים שונים.

המידע שמחזירה הפונקציה, `Time.Posix`, הוא תאריך ושעה 
בפורמט POSIX, זמן אוניברסלי מתאים, המציין את מספר השניות שחלפו מאז 1 בינואר 1970.

## ראו גם:

1. [Time.Posix documentation](https://package.elm-lang.org/packages/elm/time/latest/Time-Posix): הסביר את זה באופן מפורט יותר. 
3. [Elm’s Time module documentation](https://package.elm-lang.org/packages/elm/time/latest/Time): כל אפשרויות השימוש של מודול Time.