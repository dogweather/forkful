---
date: 2024-01-26 03:48:22.067697-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Bash \u05DC\u05D0\
  \ \u05D1\u05D0 \u05E2\u05DD \u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\u05D9\u05D0\u05D5\
  \u05EA \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DB\u05DE\u05D5 \u05DB\u05DE\u05D4 \u05E9\
  \u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05D0\u05D1\u05DC \u05D0\u05E4\
  \u05E9\u05E8 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05E7\u05D5\u05D3\
  \u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA \u05DB\u05DE\u05D5 `set -x` \u05DB\
  \u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05DE\u05D4 \u05E9\
  \u05E7\u05D5\u05E8\u05D4. \u05D0\u05D5, \u05DC\u05E9\u05D3\u05E8\u05D5\u05D2, \u05E7\
  \u05D9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.632311-06:00'
model: gpt-4-0125-preview
summary: "Bash \u05DC\u05D0 \u05D1\u05D0 \u05E2\u05DD \u05DE\u05E0\u05E4\u05D4 \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DB\u05DE\u05D5\
  \ \u05DB\u05DE\u05D4 \u05E9\u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05D0\
  \u05D1\u05DC \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05E4\u05E7\u05D5\u05D3\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA \u05DB\
  \u05DE\u05D5 `set -x` \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\
  \u05E8 \u05DE\u05D4 \u05E9\u05E7\u05D5\u05E8\u05D4."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך לעשות:
Bash לא בא עם מנפה שגיאות מובנה כמו כמה שפות אחרות, אבל אפשר להשתמש בפקודות מובנות כמו `set -x` כדי לעקוב אחר מה שקורה. או, לשדרוג, קיים `bashdb`, מנפה שגיאות ראוי לעבור דרך הקוד שלך. הנה הצצה:

```Bash
# שימוש ב-set -x לניפוי שגיאות
set -x
echo "מתחילים לנפות שגיאות"
my_var="שלום, עולם הניפוי!"
echo $my_var
set +x

# שימוש ב-bashdb
# התקנת bashdb עם מנהל החבילות שלך, למשל, apt, yum, brew.
# ניפוי של תסריט בשם my_script.sh:
bashdb my_script.sh
```

פלט כאשר מריצים עם `set -x`:
```Bash
+ echo 'מתחילים לנפות שגיאות'
מתחילים לנפות שגיאות
+ my_var='שלום, עולם הניפוי!'
+ echo 'שלום, עולם הניפוי!'
שלום, עולם הניפוי!
+ set +x
```

## צלילה עמוקה
בהיסטוריה, ניפוי שגיאות בתסריטי Bash אומר להפוך את הקוד שלך למלא בהדפסות `echo`. אבל אז הגיע `set -x`, שנתן לנו הצצה לביצוע בזמן ריצה ללא הדפסות ידניות. ולמי שמרגיש צורך ביותר שליטה, המנפה שגיאות `bashdb` צץ, מושרה מהמנפה שגיאות ל-C/C++ gdb.

בנוגע לאלטרנטיבות, מעבר לפקודות `set` (`-x`, `-v`, `-e`), אפשרויות נוספות כוללות הפניית פלט לקובץ לניתוח או שימוש בכלים חיצוניים כמו ShellCheck לניתוח סטטי.

מבחינת היישום, `set -x` קלה; זו אפשרות של Bash מקורית שמדפיסה פקודות והארגומנטים שלהן כשהן מתבצעות. `bashdb`, מצד שני, מאפשר לעבור דרך הקוד, להגדיר נקודות עצירה, ולהעריך ביטויים - דברים שנותנים לך סיכוי ליחימה נגד באגים החמקמקים יותר.

## ראה גם
- Bash Debugger Project: http://bashdb.sourceforge.net/
- "Pro Bash Programming" מאת Chris Johnson וJayant Varma לתסרוטים מתקדמים.
- ShellCheck לניתוח סטטי: https://www.shellcheck.net/
