---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:19.827885-07:00
description: "YAML, \u05E9\u05DE\u05E6\u05D9\u05D9\u05DF \u05DB\u05D9 YAML \u05D0\u05D9\
  \u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF (YAML Ain't Markup\
  \ Language), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05D4\u05E1\u05E8\u05D9\u05D0\
  \u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\u05DD \u05E9\u05E0\u05D9\u05EA\
  \u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05E2\u05D1\u05D5\u05E8\
  \ \u05E7\u05D5\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4, \u05D5\u05DB\u05DF\
  \ \u05D1\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.658614-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05DE\u05E6\u05D9\u05D9\u05DF \u05DB\u05D9 YAML \u05D0\u05D9\
  \u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF (YAML Ain't Markup\
  \ Language), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05D4\u05E1\u05E8\u05D9\u05D0\
  \u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\u05DD \u05E9\u05E0\u05D9\u05EA\
  \u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05E2\u05D1\u05D5\u05E8\
  \ \u05E7\u05D5\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4, \u05D5\u05DB\u05DF\
  \ \u05D1\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD \u05D1\u05D4\u05DD \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05E0\u05E9\u05DE\u05E8\u05D9\u05DD \u05D0\u05D5 \u05DE\
  \u05D5\u05E2\u05D1\u05E8\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
עבודה ישירה עם YAML ב-Bash מחייבת קצת יצירתיות, מאחר של-Bash אין תמיכה מובנית לפענוח YAML. עם זאת, ניתן להשתמש בכלים חיצוניים כמו `yq` (מעבד שורת פקודה YAML קל משקל ונייד) כדי להתקיים באופן יעיל עם קובצי YAML. בואו נעבור על כמה פעולות נפוצות:

### התקנת `yq`:
לפני שנתחיל בדוגמאות, ודאו ש-yq מותקן. ניתן להתקינו בדרך כלל מהמנהל האריזות שלכם, לדוגמה, באובונטו:

```bash
sudo apt-get install yq
```

או ניתן להורידו ישירות מהמאגר שלו ב-GitHub.

### קריאת ערך:
נניח שיש לכם קובץ בשם `config.yaml` עם התוכן הבא:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

כדי לקרוא את מארח המסד נתונים, ניתן להשתמש ב-`yq` כדלקמן:

```bash
yq e '.database.host' config.yaml
```

**פלט לדוגמא:**

```
localhost
```

### עדכון ערך:
כדי לעדכן את שם המשתמש ב-`config.yaml`, השתמשו בפקודת `yq eval` עם אופצית ה-`-i` (במקום):

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

ודאו את השינוי עם:

```bash
yq e '.user.name' config.yaml
```

**פלט לדוגמא:**

```
newadmin
```

### הוספת אלמנט חדש:
כדי להוסיף אלמנט חדש תחת סעיף המסד נתונים, כמו שדה חדש `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

בדיקת תוכן הקובץ תאשר את ההוספה.

### מחיקת אלמנט:
כדי להסיר את הסיסמה מתחת למשתמש:

```bash
yq e 'del(.user.password)' -i config.yaml
```

פעולה זו תסיר את שדה הסיסמה מהתצורה.

זכרו, `yq` הוא כלי עוצמתי ויש לו הרבה יותר יכולות, כולל המרת YAML ל-JSON, מיזוג קבצים, ואף מניפולציות מורכבות יותר. הפנו לתיעוד של `yq` למידע נוסף.
