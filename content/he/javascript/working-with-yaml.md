---
title:                "עובדים עם YAML"
html_title:           "Javascript: עובדים עם YAML"
simple_title:         "עובדים עם YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

מה זה ולמה?
עבודה עם YAML היא תהליך המאפשר למפתחים לאחסן ולהפעיל מידע מורכב בקוד. זהו פורמט תקין ונוח לשימוש עבור מפענחי התקציב הבינלאומי וגם לשימוש בארגונים ומפרסמים המשתמשים בנתוני יחסים ציבוריים.

איך לעשות?
דוגמאות קוד עם יציאה דוגמתית ניתן למצוא בקטעי הקוד הבאים:

```Javascript
// ייבוא ספריה לעבודה עם YAML
const YAML = require('yaml')

// מרכיבים רשימה של אובייקטים
const myList = [
  { name: 'Tom', age: 25 },
  { name: 'Jane', age: 30 },
  { name: 'John', age: 40 }
]

// יצירת מחרוזת מתוך הרשימה עם פורמט YAML
const myYAML = YAML.stringify(myList)

console.log(myYAML)
```

פלט יצירת המחרוזת השתמשתי בלמיסת לוג בכדי להדגים את הפלט. הפלט יראה כך:

```Javascript
- name: Tom
  age: 25
- name: Jane
  age: 30
- name: John
  age: 40
```

עומק עמוק:
היסטוריה: השם YAML מגיע מהמילים "יימל פחות ופחות" וזהו פורמט מידע אחנוני שנוצר בשנת 2001 על ידי קולין אינגליש בעבור תקני התקציב הבינלאומי. יישום הפורמט הוא פתוח וזמין לשימוש חופשי.

אלטרנטיבות: ישנם פורמטים נוספים להשתמש במידע מורכב כמו JSON ו-XML. אולם YAML מספק פתרונות יעילים יותר עבור מפענחי תקציב ובאפשרותו ליצור קבצי טקסט יותר קריאים ומובנים לטיפול בתוכן מורכב.

פירוט המימוש: לעבוד עם YAML, ניתן לייבא ספריית YAML עם Node.js או להשתמש בתכונות של פרטי פרוטוקול (API) למפענחי תקציב אחרים. דרך אחת לקישור עם YAML היא לקרוא פונקציית YAML.parse כדי לדפדף ולהפעיל את המידע מתוך קובץ YAML.

ראו גם:
למידע נוסף ניתן לבקר בקישורים הבאים:

- [תיעוד המפענח YAML של Node.js] (https://www.npmjs.com/package/yaml)
- [תיעוד הפרוטוקול המקורי של YAML] (https://yaml.org/spec/)
- [מדריך לכתיבת קובץ YAML] (https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)