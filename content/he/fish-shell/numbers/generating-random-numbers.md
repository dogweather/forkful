---
date: 2024-01-27 20:33:43.024802-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05DE\u05E1\u05E4\u05E8 \u05D0\u05E7\u05E8\u05D0\u05D9 \u05D1-Fish\
  \ \u05D9\u05DB\u05D5\u05DC\u05D4 \u05DC\u05D4\u05D9\u05D5\u05EA \u05E4\u05E9\u05D5\
  \u05D8\u05D4, \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\u05DC\u05D5\
  \u05D1 \u05E9\u05DC \u05DB\u05DC\u05D9 \u05D4\u05DE\u05E2\u05E8\u05DB\u05EA \u05D5\
  \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05D4-shell. \u05DC\u05D4\u05DC\u05DF \u05DB\
  \u05DE\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05D4\u05DE\u05DE\u05D7\
  \u05D9\u05E9\u05D5\u05EA \u05D0\u05D9\u05DA \u05DC\u05D9\u05D9\u05E6\u05E8 \u05DE\
  \u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:40.038202-06:00'
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8 \u05D0\u05E7\u05E8\
  \u05D0\u05D9 \u05D1-Fish \u05D9\u05DB\u05D5\u05DC\u05D4 \u05DC\u05D4\u05D9\u05D5\
  \u05EA \u05E4\u05E9\u05D5\u05D8\u05D4, \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E9\u05DC \u05DB\u05DC\u05D9 \u05D4\u05DE\u05E2\
  \u05E8\u05DB\u05EA \u05D5\u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05D4-shell."
title: "\u05D2\u05D9\u05DC\u05D5\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
weight: 12
---

## איך לעשות:
יצירת מספר אקראי ב-Fish יכולה להיות פשוטה, באמצעות שילוב של כלי המערכת ויכולות ה-shell. להלן כמה דוגמאות הממחישות איך לייצר מספרים אקראיים בתחומים מצוינים.

**לייצר מספר אקראי בין 0 ל-100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**דוגמה לפלט:**
```fish
42
```

**יצירת מספר אקראי בין שני מספרים כלשהם, נניח 50 ל-150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**דוגמה לפלט:**
```fish
103
```

**שימוש ב-random לעירבוב רשימה:**

יכול להיות שתרצו גם לערבב באקראי אלמנטים ברשימה. הנה איך תוכלו לעשות זאת:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**דוגמה לפלט:**
```fish
C
A
E
D
B
```

שימו לב, הפלט ישתנה בכל פעם שאתם מריצים את הפקודות האלה עקב טיבה של האקראיות.

## צלילה לעומק
הפונקציה `random` ב-Fish Shell מספקת ממשק נוח לשימוש ליצירת מספרים אקראיים פסבדו (מדומים). מאחורי הקלעים, היא מעטפת סביב כלי יצירת מספרים אקראיים ברמת המערכת, מציעה דרך ניידת להכניס אקראיות לתסריטים שלכם. עם זאת, חשוב לזכור שהאקראיות שמספקת `random` מספיקה לרוב משימות התסריט אך עשויה שלא לעמוד בדרישות האבטחה הקריפטוגרפיות ליישומים הזקוקים לרמה גבוהה יותר של בלתי צפיות.

להקשרים בטחוניים גבוהים, שקלו להשתמש בכלים או ספריות תכנות מוקדשים למטרות קריפטוגרפיות, אשר מספקים ערובות אקראיות חזקות יותר. עם זאת, לתסריטים כלליים ויישומים שבהם דרגת האבטחה הגבוהה ביותר לאקראיות אינה דרישה, פונקציית `random` של Fish Shell מציעה פתרון נוח ויעיל.
