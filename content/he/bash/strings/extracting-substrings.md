---
aliases:
- /he/bash/extracting-substrings/
date: 2024-01-20 17:45:24.576016-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\
  \u05D4 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D7\u05DC\u05E6\u05D9\u05DD \u05D7\
  \u05DC\u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D0\u05E8\u05D5\u05DB\u05D4 \u05D9\u05D5\u05EA\u05E8\
  . \u05EA\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05DE\u05D9\u05D3\
  \u05E2 \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9, \u05DC\u05D7\u05EA\u05D5\u05DA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05DE\u05E6\u05D5\u05D0 \u05E4\
  \u05E8\u05D9\u05D8\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA\u2026"
lastmod: 2024-02-18 23:08:53.010077
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D7\u05DC\u05E6\u05D9\u05DD \u05D7\u05DC\
  \u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D0\u05E8\u05D5\u05DB\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05EA\
  \u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05DE\u05D9\u05D3\u05E2\
  \ \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9, \u05DC\u05D7\u05EA\u05D5\u05DA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05DE\u05E6\u05D5\u05D0 \u05E4\u05E8\
  \u05D9\u05D8\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA\u2026"
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא פעולה שבה אנחנו מחלצים חלק מסוים מתוך מחרוזת ארוכה יותר. תכנתנים עושים את זה כדי לעבד מידע ספציפי, לחתוך נתונים או למצוא פריטים בתוך טקסט.

## איך לעשות:
```Bash
# יצירת מחרוזת 
str="שלום עולם, איך הכל?"

# חילוץ תת-מחרוזת על-ידי מיקום ואורך
sub1=${str:0:4}
echo "חלק ראשון: $sub1" # יודפס: חלק ראשון: שלום

# חילוץ תת-מחרוזת עד סוף המחרוזת
sub2=${str:6}
echo "חלק שני: $sub2" # יודפס: חלק שני: עולם, איך הכל?

# חילוץ בעזרת תווים דינמיים
sub3=${str#*, }
echo "חלק שלישי: $sub3" # יודפס: חלק שלישי: איך הכל?
```

## להתעמקות:
בגרסאות הראשונות של של Bash, חילוץ תת-מחרוזות היה מוגבל יותר. עם הזמן, יכולות הביטוי הרגולרי וה-globbing התפתחו לתת יותר גמישות. חלופות לחילוץ תת-מחרוזות ב-Bash כוללות שימוש ב-awk, sed, או grep, אשר יכולים להתאים למשימות מורכבות יותר בעיבוד טקסט.
המימוש של חילוץ תת-מחרוזות ב-Bash מבוסס על זמן ריצה ולא על זמן קומפילציה, כך שאפשר לבצע זאת דינמית תוך-כדי עבודת הסקריפט.

## להרחבה:
- מדריך על ביטויים רגולריים ב-Bash: https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html
- המדריך המתקדם לעבודה עם טקסט ב-Bash: http://tldp.org/LDP/abs/html/textproc.html
- אסטרטגיות חלופיות לעיבוד טקסט באמצעות כלי שורת הפקודה: https://www.cyberciti.biz/faq/category/text-processing/
