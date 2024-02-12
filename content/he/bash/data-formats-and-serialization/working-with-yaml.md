---
title:                "עבודה עם YAML"
aliases: - /he/bash/working-with-yaml.md
date:                  2024-02-03T19:25:19.827885-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שמציין כי YAML אינו שפת סימון (YAML Ain't Markup Language), הוא תקן הסריאליזציה של נתונים קריא לאדם שניתן להשתמש בו עבור קובצי תצורה, וכן ביישומים בהם נתונים נשמרים או מועברים. מתכנתים נמשכים ל-YAML עקב בהירותו ופשטותו, במיוחד בפרויקטים הדורשים תצורות מורכבות או צורך במבני נתונים ניתנים לעריכה בקלות.

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
