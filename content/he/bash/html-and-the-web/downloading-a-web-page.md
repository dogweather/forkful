---
date: 2024-01-20 17:43:48.843333-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: - How to: \u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1-`curl` \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D5\u05E8\u05D9\
  \u05D3 \u05D3\u05E3."
lastmod: '2024-03-13T22:44:39.622718-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1-`curl` \u05DB\u05D3\u05D9 \u05DC\u05D4\
  \u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

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
