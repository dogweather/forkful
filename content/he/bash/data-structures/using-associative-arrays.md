---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:19:39.947106-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E8\u05D0\u05E9\
  \u05D9\u05EA, \u05D4\u05D2\u05D3\u05D9\u05E8\u05D5 \u05DE\u05E2\u05E8\u05DA \u05D0\
  \u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9 \u05D1-Bash."
lastmod: '2024-03-13T22:44:39.608899-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D0\u05E9\u05D9\u05EA, \u05D4\u05D2\u05D3\u05D9\u05E8\u05D5 \u05DE\
  \u05E2\u05E8\u05DA \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\
  \ \u05D1-Bash."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## איך לעשות:
ראשית, הגדירו מערך אסוציאטיבי ב-Bash:

```Bash
declare -A my_array
```

לאחר מכן, תוכלו להתחיל למלא אותו בערכים, תוך שימוש במחרוזות כמפתחות:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programming"
```

לגישה לאלמנט, השתמשו במפתח שלו:

```Bash
echo ${my_array["name"]}  # מוציא: Linux Journal
```

הדפסת מפתחות וערכים באופן פשוט:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

פלט לדוגמה יכול להראות כך:

```
name: Linux Journal
topic: Programming
```

להוספה או שינוי של אלמנטים, פשוט הקצו ערך למפתח, בדומה להכנסה ההתחלתית:

```Bash
my_array["readers"]="You"
```

ולהסרת אלמנט, השתמשו ב-`unset`:

```Bash
unset my_array["topic"]
```

## טבילה עמוקה
מערכים אסוציאטיביים הוצגו בגרסה 4.0 של Bash, ולכן הם הוספה יחסית חדשה לשפה. לפני הצגתם, טיפול במערכים עם אינדקסים שאינם מספרים שלמים היה מסורבל, ולעיתים דרש פתרונות עקיפים או כלים חיצוניים כמו `awk` או `sed`.

מאחורי הקלעים, Bash מממשת מערכים אסוציאטיביים באמצעות טבלאות גיבוב. המימוש הזה מאפשר חיפוש מפתח יעיל, שנשאר די קבוע בלי תלות בגודל המערך, תכונה קריטית לביצועים בהרצת סקריפטים.

למרות שמערכים אסוציאטיביים ב-Bash מביאים הרבה כוח וגמישות לתסריטי שורת הפקודה, הם גם מציעים סט של מגבלות, כמו למשל היותם פחות נוחים לעבודה בהשוואה למערכים בשפות ברמה גבוהה יותר כמו Python או JavaScript. למשימות מתקדמות של טיפול בנתונים, עשוי להיות עדיין שווה לשקול כלים חיצוניים או שפות המתאימות יותר לעבודה.

עם זאת, למשימות תסריטיות טיפוסיות רבות, מערכים אסוציאטיביים מספקים כלי יקר ערך בערכת הכלים של מתכנת ה-Bash, ומאפשרים סקריפטים יותר קריאים וניתנים לתחזוקה על ידי שימוש במפתחות מחרוזת עם משמעות במקום אינדקסים מספריים.
