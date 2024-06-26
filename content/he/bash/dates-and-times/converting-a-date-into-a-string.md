---
date: 2024-01-20 17:36:30.004716-07:00
description: ''
lastmod: '2024-04-05T21:59:52.306416-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

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
