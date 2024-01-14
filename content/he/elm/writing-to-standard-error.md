---
title:    "Elm: כתיבה לתקליטור אנציקלופדי (Ktiva Letiklitur Anzichlopedia)"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## למה

כתיבה לפלט התקפי בגירסת Elm היא דרך נהדרת למקרי טיפול בשגיאות או לעקוב אחרי תהליכים בדיקה. 

## איך לעשות זאת

```Elm
import Debug

main =
  Debug.log "Error: Something went wrong." "This is the value to be printed to standard error."
```

הפונקציה Debug.log מאפשרת לנו להדפיס מחרוזות כותרת וערך לפלט התקפי בגירסת Elm.

## נחתום עמוק יותר

לכתוב לפלט התקפי בגירסת Elm יכול להיות כלי עזר חשוב לכתיבת בעיות בתוכנת Elm. בנוסף, זה יכול לתת לנו מידע נוסף ומומחיות לתהליכי פיתוח. כדי לקבל יותר מידע על הפונקציה Debug.log, ניתן לבדוק את התיעוד של Elm ולבדוק כיצד לבצע כתיבה לפלט התקפי בגירסת Elm עבור כל מטרה רלוונטית.

## ראה גם

- [תיעוד Elm Debug module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [סרטון הדרכה על כתיבה לפלט התקפי בגירסת Elm מ-Learn Elm in 5 Minutes](https://www.youtube.com/watch?v=oYk8CKH7eRE)
- [כתבה על כתיבה לפלט התקפי בגירסת Elm מאת דייויד צ'ייזר](https://dev.to/davidchase/error-reporting-in-elm-440j)