---
aliases:
- /he/bash/printing-debug-output/
date: 2024-01-20 17:51:55.914577-07:00
description: "\u05D3\u05D9\u05D1\u05D2\u05D9\u05E0\u05D2 \u05D4\u05D5\u05D0 \u05EA\
  \u05D4\u05DC\u05D9\u05DA \u05DC\u05D0\u05D9\u05EA\u05D5\u05E8 \u05EA\u05E7\u05DC\
  \u05D5\u05EA \u05D5\u05D1\u05E2\u05D9\u05D5\u05EA \u05D1\u05E7\u05D5\u05D3. \u05D0\
  \u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\u05E4\u05D9\u05E1\u05D9\u05DD \u05D4\u05D5\
  \u05D3\u05E2\u05D5\u05EA \u05D3\u05D9\u05D1\u05D2 \u05D1\u05DE\u05D4\u05DC\u05DA\
  \ \u05D4\u05E4\u05E2\u05DC\u05EA \u05D4\u05E7\u05D5\u05D3 \u05DB\u05D3\u05D9 \u05DC\
  \u05E8\u05D0\u05D5\u05EA \u05D1\u05D3\u05D9\u05D5\u05E7 \u05D0\u05D9\u05E4\u05D4\
  \ \u05D4\u05D5\u05D0 \u05E2\u05D5\u05E9\u05D4 \u05D0\u05EA \u05DE\u05D4 \u05E9\u05E6\
  \u05D9\u05E4\u05D9\u05E0\u05D5 \u05DE\u05DE\u05E0\u05D5 \u05D5\u05D0\u05D9\u05E4\
  \u05D4 \u05DC\u05D0."
lastmod: 2024-02-18 23:08:53.026702
model: gpt-4-1106-preview
summary: "\u05D3\u05D9\u05D1\u05D2\u05D9\u05E0\u05D2 \u05D4\u05D5\u05D0 \u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05DC\u05D0\u05D9\u05EA\u05D5\u05E8 \u05EA\u05E7\u05DC\u05D5\
  \u05EA \u05D5\u05D1\u05E2\u05D9\u05D5\u05EA \u05D1\u05E7\u05D5\u05D3. \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05D3\u05E4\u05D9\u05E1\u05D9\u05DD \u05D4\u05D5\u05D3\
  \u05E2\u05D5\u05EA \u05D3\u05D9\u05D1\u05D2 \u05D1\u05DE\u05D4\u05DC\u05DA \u05D4\
  \u05E4\u05E2\u05DC\u05EA \u05D4\u05E7\u05D5\u05D3 \u05DB\u05D3\u05D9 \u05DC\u05E8\
  \u05D0\u05D5\u05EA \u05D1\u05D3\u05D9\u05D5\u05E7 \u05D0\u05D9\u05E4\u05D4 \u05D4\
  \u05D5\u05D0 \u05E2\u05D5\u05E9\u05D4 \u05D0\u05EA \u05DE\u05D4 \u05E9\u05E6\u05D9\
  \u05E4\u05D9\u05E0\u05D5 \u05DE\u05DE\u05E0\u05D5 \u05D5\u05D0\u05D9\u05E4\u05D4\
  \ \u05DC\u05D0."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
דיבגינג הוא תהליך לאיתור תקלות ובעיות בקוד. אנחנו מדפיסים הודעות דיבג במהלך הפעלת הקוד כדי לראות בדיוק איפה הוא עושה את מה שציפינו ממנו ואיפה לא.

## איך לעשות:
```Bash
# פשוט להשתמש ב'echo' להדפסת הודעות
echo "בדיקת דיבג: המשתנה x שווה ל-$x"

# או עם 'printf' ליותר שליטה
printf "בדיקת דיבג: המערך מכיל %d איברים\n" "${#array[@]}"

# לדוגמא פלט:
# בדיקת דיבג: המשתנה x שווה ל-5
# בדיקת דיבג: המערך מכיל 3 איברים
```

## עיון נוסף
הדפסת הודעות לדיבג היא שיטה ותיקה, נפוצה בכל שפות התכנות. פעמים רבות יש אלטרנטיבות כמו מקלדות דיבג בכלים ספציפיים או בשפות תכנות מרובות המאפשרות יותר שליטה או מידע. ב-Bash, השימוש ב-'set -x' יכול להציג פקודות וערכיהן בזמן אמת, הוא שימושי מאוד אבל יכול להיות מעמיס אם יש הרבה פלט.

## ראה גם:
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
