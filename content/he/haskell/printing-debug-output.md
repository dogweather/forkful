---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט ניפוי שגיאות היא דרך שבה המתכנת יוצר "הדפסה" של מצב התוכנה בזמן ריצה. הכוונה לכך שזו דרך למצוא ולאתר שגיאות בקוד, מאחר וזה מאפשר להבחין במצב הפנימי של התוכנה בזמן שהיא מריצה.

## איך לעשות:

```Haskell
import Debug.Trace

main = print (debug "Debugging" factorial 5)
    where factorial 0 = 1
          factorial n = n * factorial (n-1)
          debug str val = trace (str ++ show val) val
```
לדוגמה, הפלט של התוכנית הזאת יוצא להיות: "Debugging120"

## הטבלה בעומק:

(1) בהקשר היסטורי, במרבית השפות התכנותית המודרניות, ניתן להשתמש במפענחים כדי לגבש את המצב הנוכחי של התוכנה. אך ב-Haskell, שהיא שפה פונקציונלית נקיים, לא קיימים תמיכה טבעית לעוקבים. על כן, ביביאה לעולם של ה-Haskell הפלט של ניפוי שגיאות נתפש כאלגחס נצח.

(2) בנוגה לאלטרנטיבות, פעמים רבות משתמשים מתכנתים בפונקציות מוינפות כמו "traceShowId" או "traceShowM" שמתחבאות תחת ה-Debug.Trace., כדי להראות את הערכים או החישובים לניף שגיאה.

(3) מבחינת ביצועים, אולי לא תיהנו תמיכה טבעית לעקובים ו sculpters, אך אם אתה שמים בשיקול דעת את הרוויחים להשתמש בהם במהלך תהליך הפיתוח, הם יכולים להפוך ניפוי השגיאות להרבה יותר נוח ואפקטיבי.

## ראה גם:

[הפלט של ניפוי שגיאות במערכת ה-Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debug-info.html)

[Trace Debugging in Haskell](https://wiki.haskell.org/Debugging)

[Debugging Haskell Code](https://typeclasses.com/techniques/debugging)