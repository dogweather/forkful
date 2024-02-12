---
title:                "עבודה עם YAML"
aliases:
- /he/typescript/working-with-yaml/
date:                  2024-02-03T19:27:15.412587-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
