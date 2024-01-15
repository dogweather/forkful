---
title:                "עבודה עם קובץ JSON"
html_title:           "Javascript: עבודה עם קובץ JSON"
simple_title:         "עבודה עם קובץ JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-json.md"
---

{{< edit_this_page >}}

למה:
מתרגשים להתחיל לעבוד עם פורמט הנתונים המאפשר כתיבה וקריאה בפשטות ויעילות רבה - JSON. הוא מאפשר תקשורת יעילה בין אפליקציות ואתרים, וזהו חלק בלתי נפרד מהפיתוח המודרני שלו.

איך לעבוד עם JSON: כדי להתחיל, נצטרך ליצור משתנה שיכיל נתונים בפורמט JSON. לדוגמה:
```Javascript
const person = {
  name: 'John',
  age: 28,
  city: 'New York'
}
```
ניתן להדפיס את הנתונים האלו בדיקה של באחד משדות האובייקט:
```Javascript
console.log(person.name) // פלט: John
```
ניתן גם להוסיף נתונים חדשים לאובייקט בדרך זו:
```Javascript
person.occupation = 'programmer'
console.log(person) // פלט: { name: 'John', age: 28, city: 'New York', occupation: 'programmer' }
```
צריך לשים לב שכל שדה באובייקט חייב להיות מוגדר בצורה תקנית כדי שהקוד יתנהג כראוי.

כדי לקבל פלט נכון כשמתעסקים עם JSON, ניתן להשתמש בפונקציית `JSON.stringify` כדי להמיר אובייקט למחרוזת בפורמט JSON:
```Javascript
const personJSON = JSON.stringify(person)
console.log(personJSON) // פלט: {"name":"John","age",28,"city":"New York","occupation":"programmer"}
```

עומק נפלא בעבודה עם JSON: JSON מכיל תמיכה רחבה בכמה פורמטים נתונים שונים כגון מספרים, מחרוזות, מערכים ועוד. בנוסף, קיימות כמה פונקציות שימושיות בתיצור וקריאת נתונים בפורמט זה, כגון `JSON.parse` ו- `JSON.parse`, שימושיות להמרת נתונים לפורמט הנוכחי שונים.

ראו גם:
- [מדריך התחלתי לעבוד עם JSON](https://developer.mozilla.org/he/docs/Learn/JavaScript/Objects/JSON)
- [מדריך להמרה מתכניתית של נתונים בין פורמטים ב-JavaScript](https://flaviocopes.com/javascript-data-types-how-to/)