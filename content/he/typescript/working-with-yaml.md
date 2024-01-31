---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט קובץ שמשמש לתצורת יישומים ונתונים, כמעט כמו ה-JSON, אך במבנה יותר קריא לאדם. תכנתים משתמשים ב-YAML בגלל הקריאות הגבוהה שלו והשילוב הפשוט עם כלים כמו Kubernetes ו-Docker.

## איך לעשות:
```TypeScript
import * as yaml from 'js-yaml';
import fs from 'fs';

// קריאת קובץ YAML
const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));

// הדפסת אובייקט מתוך קובץ YAML
console.log(doc);

// כתיבת אובייקט לקובץ YAML
const data = { title: 'מאמר על YAML', version: 1.0 };
fs.writeFileSync('./output.yaml', yaml.dump(data), 'utf8');
```
**פלט:**
בהנחה שב-'config.yaml' יש:
```yaml
version: 1
services:
  web:
    image: "node:14"
    ports:
      - "3000:3000"
```
בקונסול ירוץ:
```plaintext
{ version: 1, services: { web: { image: 'node:14', ports: [ '3000:3000' ] } } }
```
ב-'output.yaml' יכתב:
```yaml
title: מאמר על YAML
version: 1.0
```

## עומק הבנה
YAML (YAML Ain't Markup Language) פותח לראשונה ב-2001 כאלטרנטיבה ל-XML. הוא מאפשר תיאור מבנים כמו אובייקטים ומערכים בצורה נקייה וברורה. ישנם אלטרנטיבות כמו JSON ו-TOML, אך YAML נשאר פופולרי בעקבות הקריאות שלו. ב-TypeScript, נעשה שימוש בחבילות כמו `js-yaml` להמרה בין אובייקטים ל-YAML.

## ראה גם
- מסמך ה-YAML הרשמי: [YAML Spec](https://yaml.org/spec/)
- חבילת `js-yaml` ב-npm: [js-yaml](https://www.npmjs.com/package/js-yaml)
- מדריך ל-YAML ב-Kubernetes: [Kubernetes YAML Guide](https://kubernetes.io/docs/concepts/configuration/overview/#general-configuration-tips)
