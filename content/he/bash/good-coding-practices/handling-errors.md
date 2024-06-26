---
date: 2024-01-26 00:50:20.702156-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05D1\u05E6\u05E2: \u05E0\u05D9\u05D4\u05D5\
  \u05DC \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D1\u05DB\u05EA\u05D9\u05D1\u05EA\
  \ \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D1-Bash \u05DE\u05E9\u05EA\
  \u05D9\u05D9\u05DA \u05DC\u05EA\u05D7\u05D9\u05DC\u05EA \u05E2\u05D9\u05D3\u05DF\
  \ \u05E9\u05DC \u05DE\u05E2\u05D8\u05E4\u05EA Unix, \u05E9\u05DD \u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8\u05D9\u05DD \u05E2\u05DE\u05D9\u05D3\u05D9\u05DD \u05D5\u05D0\
  \u05DE\u05D9\u05E0\u05D9\u05DD \u05D4\u05D9\u05D5 (\u05D5\u05E2\u05D3\u05D9\u05D9\
  \u05DF \u05D4\u05DD) \u05D7\u05D9\u05D5\u05E0\u05D9\u05D9\u05DD \u05DC\u05E0\u05D9\
  \u05D4\u05D5\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D5\u05D0\u05D5\u05D8\u05D5\
  \u05DE\u05E6\u05D9\u05D4.\u2026"
lastmod: '2024-04-05T22:50:53.762788-06:00'
model: gpt-4-1106-preview
summary: "\u05E0\u05D9\u05D4\u05D5\u05DC \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D1\
  \u05DB\u05EA\u05D9\u05D1\u05EA \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD\
  \ \u05D1-Bash \u05DE\u05E9\u05EA\u05D9\u05D9\u05DA \u05DC\u05EA\u05D7\u05D9\u05DC\
  \u05EA \u05E2\u05D9\u05D3\u05DF \u05E9\u05DC \u05DE\u05E2\u05D8\u05E4\u05EA Unix,\
  \ \u05E9\u05DD \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05E2\u05DE\u05D9\
  \u05D3\u05D9\u05DD \u05D5\u05D0\u05DE\u05D9\u05E0\u05D9\u05DD \u05D4\u05D9\u05D5\
  \ (\u05D5\u05E2\u05D3\u05D9\u05D9\u05DF \u05D4\u05DD) \u05D7\u05D9\u05D5\u05E0\u05D9\
  \u05D9\u05DD \u05DC\u05E0\u05D9\u05D4\u05D5\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA\
  \ \u05D5\u05D0\u05D5\u05D8\u05D5\u05DE\u05E6\u05D9\u05D4."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## איך לבצע:
```Bash
#!/bin/bash

# הפניית stderr לקובץ
grep "something" file.txt 2> errors.log

# ניהול שגיאות עם סטטוסים של יציאה
if ! grep "something" file.txt; then
    echo "אופס, משהו השתבש בחיפוש אחר 'משהו'."
    exit 1
fi

# שימוש ב-trap כדי לנקות לפני יציאה עקב שגיאה
cleanup() {
  echo "מנקה קבצים זמניים..."
  rm temp_*
}

trap cleanup ERR

# שגיאה מכוונת: אין הקובץ קיים
cat temp_file.txt
```

פלט לדוגמה כאשר מתרחשת שגיאה:

```
מנקה קבצים זמניים...
cat: temp_file.txt: קובץ או ספרייה לא קיים
```

## בהרחבה
ניהול שגיאות בכתיבת סקריפטים ב-Bash משתייך לתחילת עידן של מעטפת Unix, שם סקריפטים עמידים ואמינים היו (ועדיין הם) חיוניים לניהול מערכת ואוטומציה. באופן מסורתי, שגיאות ב-Bash מנוהלות על ידי בדיקת סטטוס היציאה של פקודה, שמחזירה לפי המוסכמה 0 עבור הצלחה וערך שאינו אפס עבור כשלון.

Bash הציגה את הפקודה `trap` בתור built-in, אשר מאפשרת למשתמשים לציין פקודות שיופעלו על סימנים שונים או יציאה מהסקריפט. זה שימושי עבור משימות ניקוי או מנגנון ניהול שגיאות בכל זאת.

ישנה גם את פקודת ה-`set`, שיכולה לשנות את התנהגות Bash במקרה של שגיאות. לדוגמה, `set -e` תגרום לסקריפט לצאת מיד אם פקודה כלשהי יוצאת עם סטטוס שאינו אפס, מה שיוצר דרך להיכשל מהר ולמנוע שגיאות המשך.

אלטרנטיבות לניהול שגיאות built-in של Bash כוללות בדיקה מפורשת לקיום הקבצים, שימוש בהכלה של פקודות, או אפילו כתיבת פונקציות משלך לניהול שגיאות בצורה יותר דקדקנית.

למרות שניהול שגיאות מרובה לעיתים נתפס כמיותר לסקריפטים קטנים, זוהי פרקטיקה שיכולה לחסוך הרבה זמן בניפוי תקלות ולמנוע התנהגות בלתי צפויה גם לך וגם למשתמשים.

## ראה גם
- מדריך ל-Bash על פרמטרים של מעטפת: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- קטע על ניהול שגיאות ב-Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/exit-status.html
- מדריך מקיף ל-`trap`: https://mywiki.wooledge.org/SignalTrap

זכרו, כתיבת סקריפט היא צורת אומנות, ואופן ניהול ההחלקות והנפילות שלכם יכול לעשות את היצירה שלכם יותר עמידה. סקריפטינג שמח!
