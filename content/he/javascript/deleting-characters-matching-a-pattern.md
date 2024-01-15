---
title:                "מחיקת תווים התואמים תבנית"
html_title:           "Javascript: מחיקת תווים התואמים תבנית"
simple_title:         "מחיקת תווים התואמים תבנית"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# למה
נמחקת התקנה כרגע. התקנה החדשה של גרסת ג'אבסקריפט הנוכחית היא להסיר תווים המתאימים לתבנית מוגדרת.

## איך לעשות זאת
הנה כמה דוגמאות קוד למחיקת תווים מתאימים לתבנית נתונה:

```javascript
const string = "איך למחוק אותיים כמו אלה?";
const regex = /[אהו]/gi;
const newString = string.replace(regex, "");
console.log(newString); // ך למח אתיים כמ דל?
```

```javascript
const array = ["כרגע", "אתה", "למלאות", "החליפה"];
const regex = /[אמל]/gi;
const newArray = array.filter(word => !word.match(regex));
console.log(newArray); // ["אתה", "החיפה"]
```

```javascript
const object = {
    name: "שירה",
    age: 24,
    language: "ישראלית"
};
const regex = /[ודמ]/gi;
const newObject = Object.fromEntries(Object.entries(object).filter(entry => !entry[1].match(regex)));
console.log(newObject); // { name: "שרה", language: "ישרלית"}
```

## חקירה מעמיקה
מחיקת תווים על פי תבניות היא כלי חזק שיכול לעזור לך לעקוף עבודות יצירתיות בקוד. כאשר אתה מוצא את התבנית הנכונה עבור התווים שאתה רוצה למחוק, אתה יכול לחסוך זמן רב ומאמץ. כמו כן, היכולת למחוק תווים על פי תבניות מגדילה את יכולת הקוד שלך ומאפשרת כתיבת קוד יותר נקי ויעיל.

# ראה גם
 - [MDN פנטזיה: मारोंयो वर्ण] (https://developer.mozilla.org/he/docs/Web/JavaScript/Guide/Regular_Expressions)
- [מבוא לאתרים טכנולוגיים: מבוא לתבניות רגילות] (https://www.tau-tech.co.il/blog/javascript/regular-expressions)