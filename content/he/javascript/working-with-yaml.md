---
title:                "עבודה עם YAML"
date:                  2024-01-19
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט של קבצי תצורה קריאים לאדם, שנמצאים לעיתים קרובות בפרויקטים של קוד פתוח. תכניתנים עובדים עם YAML כי הוא אינטואיטיבי, פשוט לניתוח ומשתלב נפלא עם מערכות עיבוד אוטומטי.

## איך עושים את זה?
```javascript
const yaml = require('js-yaml');
const fs = require('fs');

// קרא YAML מקובץ
try {
  const config = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
  console.log(config);
} catch (e) {
  console.error(e);
}

// הדפסת התוצאה
// נניח שב-config.yaml יש:
// משתמש: yossi
// סיסמה: 1234
```
פלט דוגמא:
```javascript
{ 'משתמש': 'yossi', 'סיסמה': '1234' }
```

## נפילה לעומק
YAML (YAML Ain't Markup Language) נוצר ב-2001 כאלטרנטיבה ל-XML שהיה פחות נגיש לעריכה ידנית. בניגוד ל-JSON, YAML תומך בהערות ומבני נתונים מרובים. JavaScript דורש ספריות כמו `js-yaml` כדי לנתח וליצור קבצי YAML. כפי שראיתם למעלה, קריאה ועיבוד של YAML הם פעולות פשוטות.

## ראה גם
- המדריך הרשמי של YAML: https://yaml.org/spec/1.2/spec.html
- מודול `js-yaml` ב-NPM: https://www.npmjs.com/package/js-yaml
- מדריך לניתוח ויצירת YAML ב-JavaScript: https://www.tutorialspoint.com/js_yaml/index.htm
