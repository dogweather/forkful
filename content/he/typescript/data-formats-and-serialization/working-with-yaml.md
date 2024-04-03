---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:15.412587-07:00
description: "YAML, \u05E9\u05E4\u05EA \u05E1\u05D9\u05D3\u05D5\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05E9\u05EA\u05D5\u05DB\u05E0\u05E0\u05D4 \u05DC\u05D4\
  \u05D9\u05D5\u05EA \u05D9\u05D3\u05D9\u05D3\u05D5\u05EA\u05D9\u05EA \u05DC\u05D0\
  \u05D3\u05DD, \u05DE\u05E9\u05DE\u05E9\u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\
  \ \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\u05E6\
  \u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4, \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D1\
  \u05D9\u05DF-\u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD, \u05D5\u05D0\u05D7\u05E1\
  \u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E0\u05E9\u05E2\u05E0\u05D9\u05DD \u05E2\u05DC YAML \u05D1\u05D6\
  \u05DB\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.951503-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05E4\u05EA \u05E1\u05D9\u05D3\u05D5\u05E8 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E9\u05EA\u05D5\u05DB\u05E0\u05E0\u05D4 \u05DC\u05D4\u05D9\
  \u05D5\u05EA \u05D9\u05D3\u05D9\u05D3\u05D5\u05EA\u05D9\u05EA \u05DC\u05D0\u05D3\
  \u05DD, \u05DE\u05E9\u05DE\u05E9\u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\
  \u05E8\u05D5\u05D1\u05D5\u05EA \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\u05E6\u05D9\
  \ \u05EA\u05E6\u05D5\u05E8\u05D4, \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D1\u05D9\
  \u05DF-\u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD, \u05D5\u05D0\u05D7\u05E1\u05D5\
  \u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## מה ולמה?
YAML, שפת סידור נתונים שתוכננה להיות ידידותית לאדם, משמשת לעיתים קרובות עבור קבצי תצורה, הודעות בין-תהליכים, ואחסון נתונים. מתכנתים נשענים על YAML בזכות קריאותה ונוחות השימוש שלה, במיוחד כאשר מתמודדים עם נתונים מובנים מורכבים, דבר ההופך אותה לבחירה מצוינת עבור יישומים שפותחו ב-TypeScript.

## איך לעשות זאת:
עבודה עם YAML ב-TypeScript בדרך כלל כוללת פרסור של תוכן YAML לאובייקטים של JavaScript ואפשר גם המרה של אובייקטים של JavaScript בחזרה ל-YAML. זה דורש מפענח; אחת הבחירות הפופולריות היא `js-yaml`, ספריה שניתן לשלב בקלות בפרויקטים של TypeScript.

### התקנת js-yaml
ראשית, הוספו את `js-yaml` לפרויקט שלכם:

```bash
npm install js-yaml
```

### פרסור YAML לאובייקט JavaScript
נניח שיש לכם קובץ YAML `config.yaml` עם התוכן הבא:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

תוכלו לקרוא ולפרסר קובץ זה לאובייקט של JavaScript כדלהלן:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// טעינה ופרסור של קובץ ה-YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**דוגמת פלט:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### המרת אובייקט JavaScript ל-YAML
אם אתם צריכים לעשות את הכיוון השני ולהמיר אובייקט של JavaScript למחרוזת YAML, ניתן להשתמש ב-`js-yaml` כך:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**דוגמת פלט:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

קטע קוד זה ממיר אובייקט JavaScript למחרוזת YAML ומציג אותה. בפועל, ייתכן שתכתבו זאת בחזרה לקובץ או תשתמשו בה בחלקים אחרים של היישום שלכם.
