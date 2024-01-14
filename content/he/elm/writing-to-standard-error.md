---
title:    "Elm: כתיבה לשגיאת הסטנדרט"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

למה לכתוב לתקליטור שגיאות מתקנים יותר עדיף מלהדפיס שגיאות ישירות לטרמינל? כתיבה לתקליטור שגיאות עוזרת לבדוק ולמצוא בעיות בקוד יותר בקלות כיוון שהקלט הינו פתוח וברור.

## איך לעשות זאת:

```elm
import Debug

itemCount : Int
itemCount = 5

totalPrice : Int
totalPrice = 10

Debug.log "Something went wrong!" (totalPrice / itemCount)
```

פלט:
```
Something went wrong!: 2
```

## חפירה עמוקה

לכותבי קוד מקצועיים ייתכן שימשיכו לשכפל את הפלט והקלט, לעתים קרובות לקרוא קוד אחר שמתבטא בפלט כפול. למרבה המזל, כתיבה לטרימנל יעזור לך לזהות את השגיאות השונות ולעקוב אחרי התקליטור בקלות יותר. זה משמעותי יותר במיוחד כאשר מדובר בקוד עמוק יותר ומורכב, כאשר קשה יותר להבין את השגיאות השונות שמתנגשות.

## ראה גם

- [Debug documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Using console.log in Elm](https://noredinktech.wordpress.com/2016/08/08/elm-console-log/)
- [Learning to debug Elm](https://blog.janestreet.com/learning-to-debug-elm/)