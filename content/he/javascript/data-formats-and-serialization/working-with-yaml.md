---
title:                "עבודה עם YAML"
aliases: - /he/javascript/working-with-yaml.md
date:                  2024-02-03T19:26:52.494557-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שמקוצר מ-"YAML Ain't Markup Language", הוא פורמט לערוך נתונים קריא לאדם. תכנתים לעיתים קרובות משתמשים בו לקובצי הגדרה ולחילופי נתונים בין שפות מכיוון שהוא פשוט וקריא יותר בהשוואה ל-JSON או XML.

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
