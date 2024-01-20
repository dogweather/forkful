---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פרסון HTML הוא התהליך שבו נקרא קוד HTML ומפשטים אותו למבנה נתונים שמחשבים יכולים לעבוד איתו. מתכנתים מבצעים פעולה זו כשהם רוצים לאפשר לתוכניהם לתקשר או לשיפור עמוד רשת.

## איך לעשות:
אם נרצה לפרסם HTML עם Fish Shell, נכתוב פונקציה שכך עובדת:

```Fish Shell
function parse_html
    set html (curl -s $argv[1])
    echo $html | pup 'p text{}'
end
```
בריצה, הפונקציה תחזיר את הטקסט בתוך כל הפסקאות שבעמוד:
```Fish Shell
> parse_html https://he.wikipedia.org/wiki/HTML
ה-HTML (HyperText Markup Language לשמו המלא באנגלית, בתרגום חופשי לעברית: שפת סימון היפר-טקסט) היא שפת סימון שמשמשת ל...
```

## צלילה עמוקה:
הפרסון של HTML היה חשוב בהיסטוריה של המחשוב מאז שה-WWW נוצר ב-1989. בשפת פיש, אנו מתאימים את מנגנון הפרסינג שלנו ל-HTML לאמצעות ספריית `pup` שמפשטת את הפרסנים לאובייקטים JS.

חלופות ל-Pup כוללות Jsoup, Beautiful Soup ו-lxml. אף אחת מהן אינה קיימת לפיש, אך כולן היו מתאימות אם כתבתם ב-Python או ב-Java.

## ראה גם:
- [דף הבית של Pup](https://github.com/ericchiang/pup)
- [מדריך לשפת Pup](https://github.com/ericchiang/pup/blob/master/README.md)
- [חמישה דרכים לפרסם HTML](https://www.scrapingbee.com/blog/five-ways-to-extract-data-from-html/)
- [Fish Shell באתר הרשמי](https://fishshell.com/)