---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:52.494557-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1-JavaScript, \u05E2\u05D1\u05D5\u05D3\
  \u05D4 \u05E2\u05DD YAML \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DE\u05DB\u05D9\u05D5\
  \u05D5\u05DF \u05E9\u05D4\u05E9\u05E4\u05D4 \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05DE\u05E4\u05E2\u05E0\u05D7 \u05DE\u05D5\u05D1\u05E0\u05D4\
  \ \u05DC-YAML. \u05D0\u05D7\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05D4\
  \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05D5\u05EA \u05D1\u05D9\u05D5\u05EA\u05E8\
  \ \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5\u2026"
lastmod: '2024-03-13T22:44:40.008759-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-JavaScript, \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05D1\
  \u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E6\u05D3 \u05E9\
  \u05DC\u05D9\u05E9\u05D9 \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05E9\
  \u05E4\u05D4 \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05DE\u05E4\
  \u05E2\u05E0\u05D7 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DC-YAML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך ל:
ב-JavaScript, עבודה עם YAML בדרך כלל כוללת שימוש בספרייה צד שלישי מכיוון שהשפה אינה כוללת מפענח מובנה ל-YAML. אחת הספריות הפופולריות ביותר למטרה זו היא `js-yaml`. אתם יכולים להשתמש ב-`js-yaml` לפרסור YAML לאובייקטים של JavaScript ולהיפך.

ראשית, עליכם להתקין את `js-yaml`:

```bash
npm install js-yaml
```

לאחר מכן, אתם יכולים להשתמש בו בפרויקטים שלכם. הנה איך אפשר לטעון קובץ YAML ולפרסר אותו לאובייקט של JavaScript:

```javascript
// דרוש מודול של js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// טוען YAML מקובץ
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

אם הקובץ `config.yaml` שלכם נראה כך:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

הפלט יהיה:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

לעשות את ההפך, המרת אובייקט של JavaScript למחרוזת YAML:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

קוד זה יייצר:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

באמצעות `js-yaml`, תוכלו לשלב בקלות הערוך והמרה של YAML בפרויקטים של JavaScript שלכם, ובכך לשפר את החליפיות של נתונים וניהול הגדרות.
