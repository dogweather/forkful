---
date: 2024-01-20 17:37:03.613269-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D5 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\
  \u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\
  \u05D8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E7\u05E8\u05D9\
  \u05D0 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9, \u05DC\u05E9\u05DE\u05D5\u05E8 \u05D1\
  \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\
  \u05D9\u05DD \u05DC\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD,\u2026"
lastmod: '2024-02-25T18:49:38.310966-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D6\u05D5 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\
  \u05D4 \u05D0\u05E0\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05E4\u05D5\u05E8\u05DE\
  \u05D8 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\u05D8\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E7\u05E8\u05D9\u05D0\
  \ \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9, \u05DC\u05E9\u05DE\u05D5\u05E8 \u05D1\u05E4\
  \u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05D9\
  \u05DD \u05DC\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD,\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת זו פעולה שבה אנו משנים פורמט של תאריך לטקסט. מתכנתים עושים זאת כדי להציג תאריכים באופן קריא למשתמש, לשמור בפורמטים סטנדרטיים לנתונים, או לעיבוד תאריכים בתוכניות שלהם.

## איך לעשות:
ב-Fish המרת תאריך למחרוזת היא פשוטה. קודם כל, תזדקקו לפקודה `date`. נתחיל עם הדוגמה הבאה:

```Fish Shell
set current_date (date "+%Y-%m-%d")
echo $current_date
```

פלט דוגמה:
```
2023-03-19
```

לייצור תבניות מורכבות יותר, שנו את הפורמט שאחרי `+`:

```Fish Shell
set custom_format (date "+%A, %d %B %Y %H:%M:%S %Z")
echo $custom_format
```

פלט דוגמה:
```
Sunday, 19 March 2023 15:45:12 IST
```

## מעמיקים יותר
יש היסטוריה ארוכה של המרות תאריך למחרוזת במערכות יוניקס ולינוקס; זו אחת המשימות הבסיסיות בסקריפטינג של מערכת. Fish Shell מספקת ממשק נוח וקריא יחסית לפקודות אלו.

האלטרנטיבות כוללות שימוש בסקריפטים אחרים כמו bash או zsh, אבל Fish מציע סינטקס ייחודי וכלים מובנים שיכולים להקל על העבודה עם תאריכים וזמנים.

למשל, Fish משתמשת בפקודת `date`, אך מאפשרת להרחיב את שימושית על ידי המובנה `string` למניפולציות של מחרוזות. זה מאפשר המרה נוחה של תאריכים לפורמטים מורכבים, גזירה וחיתוך של חלקים מתוך מחרוזת התאריך, ועוד.

בנוסף, Fish מאפשרת התאמת צורת הקוד והטיפול במשתנים באופן אינטואיטיבי יותר עבור רבים.

## ראו גם
- [תיעוד רשמי של Fish Shell](https://fishshell.com/docs/current/index.html)
- [תיעוד על פקודת `date` בלינוקס](https://man7.org/linux/man-pages/man1/date.1.html)
- [מדריך לסינטקס הפקודות ב-Fish](https://fishshell.com/docs/current/commands.html)
