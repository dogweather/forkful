---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "PHP: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# במה ומדוע?
חישוב תאריך בעתיד או בעבר הוא פעולה שמאפשרת למתכנתים להשתמש בכלים שלהם בכדי לחשב את תאריך מסוים בעתיד או בעבר. זה נחשב לכלי חשוב בעבודתם של מתכנתים ומסייע להם לקבוע תאריכים מדוייקים להתאמה לדרישות הפרויקט שלהם.

## איך לעשות?
הכי פשוט לחשב תאריך בעתיד או בעבר הוא להשתמש בפונקציה `strtotime()` של PHP. ניתן לתת לפונקציה זו שני פרמטרים - תאריך ומחרוזת שמציינת את התאריך המבוקש (למשל "tomorrow" לתאריך מחר או "next Monday" ליום