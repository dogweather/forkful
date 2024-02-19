---
aliases:
- /he/fish-shell/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:47.539386-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E8\u05E1\u05D5\u05DF (\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7) \u05D5\
  \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E7\u05D1\
  \u05E6\u05D9 YAML (YAML Ain't Markup Language - YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\
  \u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF), \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E9\
  \u05DC \u05E1\u05D9\u05D3\u05E8\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DC\u05E7\
  \u05D1\u05E6\u05D9\u2026"
lastmod: 2024-02-18 23:08:53.316875
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E8\u05E1\u05D5\u05DF (\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7) \u05D5\
  \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E7\u05D1\
  \u05E6\u05D9 YAML (YAML Ain't Markup Language - YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\
  \u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF), \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E9\
  \u05DC \u05E1\u05D9\u05D3\u05E8\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DC\u05E7\
  \u05D1\u05E6\u05D9\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם YAML כוללת פרסון (פיענוח) ומניפולציה של קבצי YAML (YAML Ain't Markup Language - YAML אינו שפת סימון), פורמט של סידרול נתונים שנמצא בשימוש לקבצי הגדרות (קונפיגורציה), ב-Fish Shell. מתכנתים עושים זאת על מנת לאוטמט ולהגדיר יישומים או שירותים ביעילות בהקשר של סביבות של (חלונית פקודה), מה שמקל על משימות כמו ניהול הגדרות והקצאת משאבים.

## איך לעשות:
Fish Shell אינו כולל תמיכה מובנית לפרסון של YAML, אך ניתן להשתמש בכלים צד שלישי כמו `yq` (מעבד שורת פקודה קל משקל ונייד ל-YAML) כדי לטפל בנתוני YAML.

**התקנת yq (אם לא הותקן בעבר):**
```fish
sudo apt-get install yq
```

**קריאת ערך מתוך קובץ YAML:**
נניח שיש לכם קובץ YAML בשם `config.yaml` עם התוכן הבא:
```yaml
database:
  host: localhost
  port: 3306
```

כדי לקרוא את מארח המסד נתונים, תשתמשו ב:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**דוגמת פלט:**
```
localhost
```

**עדכון ערך בקובץ YAML:**
כדי לעדכן את `port` ל-`5432`, השתמשו ב:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**האמתו את העדכון:**
```fish
yq e '.database.port' config.yaml
```
**דוגמת פלט:**
```
5432
```

**כתיבת קובץ YAML חדש:**
ליצירת `new_config.yaml` חדש עם תוכן מוגדר מראש:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
זה משתמש ב-`yq` כדי לעבד ולהדפיס בצורה נאה (-P flag) מחרוזת לקובץ YAML חדש.

**פרסון מבנים מורכבים:**
אם יש לכם קובץ YAML מורכב יותר ואתם צריכים לאחזר מערכים או אובייקטים מקוננים, תוכלו:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**דוגמת פלט:**
```
server1
server2
```
באמצעות `yq`, Fish Shell הופך את הניווט והמניפולציה במסמכי YAML לפשוטים לשימוש במגוון משימות אוטומטיות וקונפיגורציה.
