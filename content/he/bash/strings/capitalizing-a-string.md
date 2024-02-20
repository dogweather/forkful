---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:17.090911-07:00
description: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D4\u05D0\u05D5\u05EA \u05D4\u05E8\
  \u05D0\u05E9\u05D5\u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\
  -Bash \u05DB\u05D5\u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\
  \u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05D4\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  \ \u05EA\u05D5\u05DA \u05D4\u05E9\u05D0\u05E8\u05EA \u05E9\u05D0\u05E8 \u05D4\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9\
  . \u05E9\u05D9\u05D8\u05D4 \u05D6\u05D5 \u05DE\u05E9\u05DE\u05E9\u05EA \u05DC\u05E2\
  \u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DC\u05E6\u05D5\
  \u05E8\u05DA \u05E2\u05D9\u05E6\u05D5\u05D1\u2026"
lastmod: 2024-02-19 22:04:58.843353
model: gpt-4-0125-preview
summary: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D4\u05D0\u05D5\u05EA \u05D4\u05E8\u05D0\
  \u05E9\u05D5\u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Bash\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\u05EA\
  \u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05D4\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05EA\
  \u05D5\u05DA \u05D4\u05E9\u05D0\u05E8\u05EA \u05E9\u05D0\u05E8 \u05D4\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9. \u05E9\
  \u05D9\u05D8\u05D4 \u05D6\u05D5 \u05DE\u05E9\u05DE\u05E9\u05EA \u05DC\u05E2\u05D9\
  \u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DC\u05E6\u05D5\u05E8\
  \u05DA \u05E2\u05D9\u05E6\u05D5\u05D1\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הגדלת האות הראשונה במחרוזת ב-Bash כוללת המרה של התו הראשון של המחרוזת לאות גדולה תוך השארת שאר המחרוזת ללא שינוי. שיטה זו משמשת לעיתים קרובות לצורך עיצוב פלט או כדי לעמוד בקונבנציות קידוד הדורשות שמחרוזות מסוימות יתחילו באות ראשית, לצורך קריאות או העדפות סגנוניות.

## איך לעשות:

Bash אינו מכיל פונקציה מובנית במיוחד להגדלת אותיות במחרוזת, אך ניתן לבצע משימה זו באמצעות הרחבת פרמטרים או כלים חיצוניים כמו `awk`. הנה כמה דרכים להגדלת האות הראשונה במחרוזת ב-Bash:

**באמצעות הרחבת פרמטרים:**

שיטה זו מתעסקת במניפולציה של המחרוזת ישירות בשל.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
פלט:
```
Hello world
```

**באמצעות `awk`:**

`awk` הוא כלי עיבוד טקסט חזק הזמין ברוב מערכות ההפעלה הדומות ל-Unix, אשר ניתן לנצל לצורך הגדלת מחרוזות.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
פלט:
```
Hello world
```

**באמצעות `sed`:**

לגישה יותר מסורתית, ניתן להשתמש ב-`sed` כדי להגדיל את האות הראשונה של מחרוזת. עם זאת, היא מורכבת יותר בהשוואה לשיטות הקודמות.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
פלט:
```
Hello world
```

קטעי הקוד הללו מדגימים איך להגדיל את האות הראשונה של מחרוזת ב-Bash, ומדגישים את הגמישות של כתיבת קוד בשל כאשר מתעסקים בעיבוד טקסט.
