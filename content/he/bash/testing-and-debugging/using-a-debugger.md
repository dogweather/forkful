---
aliases:
- /he/bash/using-a-debugger/
date: 2024-01-26 03:48:22.067697-07:00
description: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA (Debugger) \u05D1-Bash \u05D0\u05D5\u05DE\u05E8 \u05DC\
  \u05E0\u05E6\u05DC \u05DB\u05DC\u05D9\u05DD \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D5\
  \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D1\u05E2\u05D9\u05D5\u05EA \u05D1\u05EA\u05E1\
  \u05E8\u05D9\u05D8\u05D9\u05DD \u05E9\u05DC\u05DA, \u05DB\u05DE\u05D5 \u05DC\u05DC\
  \u05DB\u05D5\u05D3 \u05D1\u05D0\u05D2\u05D9\u05DD \u05E9\u05D2\u05D5\u05E8\u05DE\
  \u05D9\u05DD \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DC\u05E7\u05E8\u05D5\
  \u05E1 \u05D0\u05D5 \u05DC\u05D4\u05EA\u05E0\u05D4\u05D2 \u05D1\u05E6\u05D5\u05E8\
  \u05D4 \u05DE\u05E1\u05EA\u05D5\u05E8\u05D9\u05EA.\u2026"
lastmod: 2024-02-18 23:08:53.028747
model: gpt-4-0125-preview
summary: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA (Debugger) \u05D1-Bash \u05D0\u05D5\u05DE\u05E8 \u05DC\u05E0\
  \u05E6\u05DC \u05DB\u05DC\u05D9\u05DD \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D5\u05DC\
  \u05DE\u05E6\u05D5\u05D0 \u05D1\u05E2\u05D9\u05D5\u05EA \u05D1\u05EA\u05E1\u05E8\
  \u05D9\u05D8\u05D9\u05DD \u05E9\u05DC\u05DA, \u05DB\u05DE\u05D5 \u05DC\u05DC\u05DB\
  \u05D5\u05D3 \u05D1\u05D0\u05D2\u05D9\u05DD \u05E9\u05D2\u05D5\u05E8\u05DE\u05D9\
  \u05DD \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DC\u05E7\u05E8\u05D5\u05E1\
  \ \u05D0\u05D5 \u05DC\u05D4\u05EA\u05E0\u05D4\u05D2 \u05D1\u05E6\u05D5\u05E8\u05D4\
  \ \u05DE\u05E1\u05EA\u05D5\u05E8\u05D9\u05EA.\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש במנפה שגיאות (Debugger) ב-Bash אומר לנצל כלים לבדוק ולמצוא בעיות בתסריטים שלך, כמו ללכוד באגים שגורמים לקוד שלך לקרוס או להתנהג בצורה מסתורית. מתכנתים עושים זאת מכיוון שזה הרבה יותר חכם לתפוס שגיאות לפני שהן גורמות להרס בסביבה חיה.

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
