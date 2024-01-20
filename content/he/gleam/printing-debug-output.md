---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט של ניפוי הוא אמצעי שבו מתכנתים מדפיסים מידע לקונסולה לניפוי באגים. זה חשוב כי זה נותן למתכנתים להבין טוב יותר את ההתנהגות של התוכנה בזמן ריצה, ולזהות בעיות מהר יותר.

## איך לעשות:

הנה דוגמה של הדפסת פלט של ניפוי בגלים:

```Gleam 
import gleam/io

fn main() {
    io.debug("הודעת ניפוי")
}
```

אם תריצו את הקוד הזה, תשמעו את ההודעה "הודעת ניפוי" נדפסת בקונסולה.

## Deep Dive 

במקור, הדפסת פלט של ניפוי שימשה בעיקר לניפוי באגים בזמן שהתוכנה פעלה. אך עם הזמן, מתכנתים התחילו לשים לב שהיא משמשת תמיד כאמצעי עוקב להעמיד את התוכנה במצב שבו הם יכולים לראות את כל מה שקורה בפנים.

בקרב הלפתחות המשחק יכולות להגיע לפרשניות של "println()" או "console.log()". זה נוגע גם למספר חלופה של ספריות להדפסת פלט של ניפוי ולניתוח יומנים. 

הפונקציה `io.debug/1` של Gleam מנצלת את היכולת של Erlang להדפיס מסרים לקונסולה מבלי להשפיע על ביצועים או על פעילות נומלה של התוכנה.

## ראה גם:

אתה יכול ללמוד עוד על ניפוי ב-Gleam ועל הדפסת הפלט דרך המקורות הבאים:

[Gleam's Debug documentation](https://gleam.run/documentation/debugging-and-testing/) 
[Erlang's IO module, which Gleam's io module is based on](http://erlang.org/doc/man/io.html) 
[Debugging Techniques in functional programming](https://www.infoq.com/articles/debug-techniques-functional/)