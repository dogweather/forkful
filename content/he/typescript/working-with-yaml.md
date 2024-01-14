---
title:                "TypeScript: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות המודרני, YAML הפך לפורמט נתונים נפוץ ביותר לכתיבת קבצי הגדרות. יתרונו העיקרי הוא הקריאות והכתיבה הפשוטים, המאפשרים למתכנתים לנהל קבצים מבלי להתקשר לקוד. לכן, ידע בתכנות ב-YAML הפך לכלי חשוב לכל מתכנת.

## כיצד לעבוד עם YAML ב-TypeScript

השילוב של יכולות הגבוהות של TypeScript עם היימל מאפשר למתכנתים ליצור קבצי הגדרות בקלות רבה ובצורה ברורה יותר. בחלק זה, נדגים כיצד לעבוד עם יימל ב-TypeScript ונראה דוגמאות של קוד ופלט.

```TypeScript
// דוגמה לכתיבת ערכים מהטיים
const yaml =
  `אהלן: עולם!
   שלום: מתכנט!
   מספרים:
     - 1
     - 2
     - 3`;

const data = YAML.parse(yaml); // המרת ה-String לאובייקט

console.log(data); // {האהלן: עולם!, שלום: מתכנט!, מספרים: [1, 2, 3]}
```

בנוסף, ניתן ליצור קבצי YAML בקלות מתוך אובייקטים בטיפוס המתאים:

```TypeScript
// דוגמה ליצירת קובץ YAML מהטייפסקריפט
const data = {
  אהלן: 'עולם!',
  שלום: 'מתכנט!',
  מספרים: [1, 2, 3]
};

const yaml = YAML.stringify(data); // המרת האובייקט ל-String

console.log(yaml); // אהלן: עולם!
                    // שלום: מתכנט!
                    // מספרים:
                    //   - 1
                    //   - 2
                    //   - 3
```

כדי לעבוד עם קבצי YAML ייצוגיים, ניתן להשתמש ב-Parse עם אפשרויות נוספות כגון נקודת התחלה וסיום היימל.

```TypeScript
// דוגמה לחלוקת קבצי YAML ייצוגיים לפי נקודת התחלה וסיום
const yaml =
`--- קבצי הגדרות לתוכניות ---
אהלן: