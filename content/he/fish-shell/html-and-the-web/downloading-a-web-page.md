---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:43:59.572232-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט זה פשוט לשלוף תוכן מתוך כתובת URL ולשמור אותו במקומי. תכניתנים עושים את זה לאיסוף נתונים, בדיקות אוטומטיות, או פשוט כדי להשיג את התוכן בלי דפדפן.

## איך לעשות:
כדי להוריד דף אינטרנט ב-Fish, ניתן להשתמש בפקודות פשוטות כמו `curl` או `wget`. דוגמה:

```Fish Shell
curl https://example.com -o example_page.html
```

זה ישמור את דף האינטרנט של example.com בקובץ בשם example_page.html. אם רוצים לראות את התוכן בטרמינל:

```Fish Shell
curl https://example.com
```

ותקבלו פלט בסגנון:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## צלילה לעומק:
בשנות ה-90', עם הופעת האינטרנט, צריך היה דרך להוריד קבצים מרחוק. אז הומצאו פקודות כמו `curl` ו־`wget`. שתיהן עושות את אותו הדבר באופן בסיסי, אבל יש להן אופציות שונות לשימושים מתקדמים. למשל, `curl` היא נפוצה לשליחת בקשות HTTP בצורות שונות, בעוד ש-`wget` ידועה ביכולתה להוריד אתרים שלמים לשימוש לא מקוון. ניתן גם לעשות כמה פעולות עיבוד כמו ניתוח מידע (parsing) של התוכן באמצעות תוכניות אחרות.

## ראו גם:
- מדריך ל-Fish Shell: https://fishshell.com/docs/current/index.html
- מידע מעמיק על `curl`: https://curl.se/docs/
- מידע מעמיק על `wget`: https://www.gnu.org/software/wget/manual/wget.html
- הדרכה על ניתוח HTML ב-Fish באמצעות `pup`: https://github.com/ericchiang/pup
