---
title:                "שימוש בביטויים רגילים"
html_title:           "Fish Shell: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
בייטים משמשים לזיהוי תבניות טקסט חוזרות ונשנות. מתכנתים משתמשים בזה כדי לאפשר לחלק מהקוד שלהם לעבוד באופן גנרלי ומשתנה.

## איך לעשות:
```Fish Shell
# איך להשתמש בביטויים רגילים ב-Fish Shell
echo "Hello, World!" | grep -o 'World'
```
הפלט של משפט זה יהיה:
```
World
```
## צלילה עמוקה:
השרש של ביטויים רגולריים מגיע מהתיאוריה המתמטית של האוטומטה. כיום, מתכנתים משתמשים בהם במגוון שפות תכנות שונות. נעשתה בהם שיפורים משמעותיים מאז שנוצרו כדי לסייע למתכנתים לעבוד באופן מהיר ויעיל יותר.
אולם ישן לבדוק חלופות, משמע שאין אתה מסתמך מדי על ביטויים רגולריים. לגישה לזיהוי תבניות של טקסט, שפות תכנות עכשוויות כגון Python, Java, או JavaScript מציעות מגוון עזרים.
סריקת השורה באופן אחריותי מתאמת גם כאשר אתה מיישם רגיל אמיתי במעטפת Fish. 

## ראה גם:
1. [בחנינו מדריך בקרה אמיתי](https://www.regular-expressions.info/)
2. [Fish shell המדריך הרשמי](https://fishshell.com/docs/current/tutorial.html)
3. [MDN Web Docs המדריך רשמי](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)