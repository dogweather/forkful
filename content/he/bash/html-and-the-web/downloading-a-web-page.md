---
title:                "הורדת דף אינטרנט"
aliases:
- /he/bash/downloading-a-web-page.md
date:                  2024-01-20T17:43:48.843333-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה? - What & Why?

להוריד דף אינטרנט זה לשמור עליו במחשב שלך. תכנותים עושים את זה לניתוח נתונים, בדיקות אוטומטיות, או פשוט לשימוש אופליין.

## איך לעשות: - How to:

השתמש ב-`curl` כדי להוריד דף:

```Bash
curl http://example.com -o page.html
```

תוצאה:

```Bash
# page.html נוצר עם התוכן של example.com
```

או תוכלו להשתמש ב-`wget` למטרה דומה:

```Bash
wget http://example.com
```

תוצאה:

```Bash
# הורדה: example.com => 'index.html'
```

## צלילה לעומק - Deep Dive:

חוקרים ופיתחו כלים כמו `curl` ו-`wget` מתחילת תקופת האינטרנט. הם מאפשרים אוטומציה וניהול של קבצים מגוונים. ב-`curl` יש אופציות רבות למיפוי התקשורת. `wget` אוהב לשמש להורדות רקורסיביות ומראות של אתרים כוללים. שניהם ניתנים לשילוב בתוך סקריפטים מורכבים.

## ראה גם: - See Also:

- מדריך ל-`curl`: https://curl.se/docs/manpage.html
- מדריך ל-`wget`: https://www.gnu.org/software/wget/manual/wget.html
- בלוג עם טיפים ל-`curl` ו-`wget`: https://daniel.haxx.se/blog/
