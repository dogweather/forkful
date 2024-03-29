---
date: 2024-01-20 17:36:30.004716-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1-Bash \u05D4\u05D9\u05D0\
  \ \u05E4\u05E8\u05D5\u05E6\u05D3\u05D5\u05E8\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\
  \u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05D5\u05DC\u05D0\
  \u05D7\u05E1\u05DF \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05EA\u05E6\
  \u05D5\u05E8\u05D4 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D5\u05E0\u05D5\u05D7\u05D4\
  \ \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05E4\u05E9\u05D8 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05DC\u05E7\u05DC\u05D5\
  \u05D8 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.643973-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1-Bash \u05D4\u05D9\u05D0 \u05E4\
  \u05E8\u05D5\u05E6\u05D3\u05D5\u05E8\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05E0\u05D5 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05D5\u05DC\u05D0\u05D7\u05E1\
  \u05DF \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05EA\u05E6\u05D5\u05E8\
  \u05D4 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D5\u05E0\u05D5\u05D7\u05D4 \u05DC\u05E2\
  \u05D9\u05D1\u05D5\u05D3. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E4\
  \u05E9\u05D8 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05DC\u05E7\u05DC\u05D5\u05D8 \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריכים למחרוזות ב-Bash היא פרוצדורה שמאפשרת לנו להציג ולאחסן תאריכים בתצורה קריאה ונוחה לעיבוד. תכניתנים עושים את זה כדי לפשט תיעוד, לקלוט תאריכים בקובצי קלט חיצוניים, ולהציג תאריכים למשתמשים באופן ברור.

## איך לעשות:

```Bash
# קבלת התאריך והשעה הנוכחיים
current_date=$(date)
echo $current_date
```

Output:
```
Thu Mar  3 10:26:41 UTC 2021
```

```Bash
# המרת תאריך ושעה למחרוזת מותאמת אישית
formatted_date=$(date +"%d-%m-%Y %H:%M:%S")
echo $formatted_date
```

Output:
```
03-03-2021 10:26:41
```

## עיון רחב:

### הקשר ההיסטורי
הפקודה `date` נוצרה בתחילת ימי מערכות ההפעלה יוניקס ושימשה להצגת והגדרת תאריכים ושעות במערכת. 

### אלטרנטיבות
קיימות כלים חיצוניים כמו `datetime` ב-Python ו`Date` ב-JavaScript שמאפשרים גמישות רבה יותר בעבודה עם תאריכים, אך לא תמיד יש צורך לקפוץ לשפה אחרת כש-Bash מציע פתרון מהיר ויעיל. 

### פרטי יישום
בעת שימוש בפקודת `date`, שילוב של מילות קוד (format specifiers) כמו `%Y` לשנה, `%m` לחודש ו-%d` ליום מאפשר התאמה אישית של הפלט. שימוש במרכאות כפולות סביב הפורמט מבטיח שמחרוזות התאריך יופיעו כפי שציינו, בלי שמחרוזות פורמט מיוחדות יופרשו בטעות כפקודות Bash.

## ראה גם:

- למידע נוסף על פורמט התאריך ב-Bash, היכנסו ל-[GNU Coreutils - Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- להיכרות עם פורמט הזמן והתאריך המלא ב-Bash, בקרו ב-[man7.org](https://man7.org/linux/man-pages/man1/date.1.html)
