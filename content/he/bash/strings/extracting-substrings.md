---
date: 2024-01-20 17:45:24.576016-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D2\u05E8\
  \u05E1\u05D0\u05D5\u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\u05EA \u05E9\
  \u05DC \u05E9\u05DC Bash, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D4 \u05DE\u05D5\u05D2\u05D1\u05DC\
  \ \u05D9\u05D5\u05EA\u05E8. \u05E2\u05DD \u05D4\u05D6\u05DE\u05DF, \u05D9\u05DB\u05D5\
  \u05DC\u05D5\u05EA \u05D4\u05D1\u05D9\u05D8\u05D5\u05D9 \u05D4\u05E8\u05D2\u05D5\
  \u05DC\u05E8\u05D9 \u05D5\u05D4-globbing \u05D4\u05EA\u05E4\u05EA\u05D7\u05D5 \u05DC\
  \u05EA\u05EA \u05D9\u05D5\u05EA\u05E8 \u05D2\u05DE\u05D9\u05E9\u05D5\u05EA. \u05D7\
  \u05DC\u05D5\u05E4\u05D5\u05EA \u05DC\u05D7\u05D9\u05DC\u05D5\u05E5\u2026"
lastmod: '2024-04-05T21:53:40.728155-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D2\u05E8\u05E1\u05D0\u05D5\u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05E0\u05D5\u05EA \u05E9\u05DC \u05E9\u05DC Bash, \u05D7\u05D9\u05DC\u05D5\u05E5\
  \ \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D4 \u05DE\
  \u05D5\u05D2\u05D1\u05DC \u05D9\u05D5\u05EA\u05E8."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

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
