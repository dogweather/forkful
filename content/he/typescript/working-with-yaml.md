---
title:                "עבודה עם yaml"
html_title:           "TypeScript: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

בואו נדבר על YAML - לשימוש בטיפוסקריפט

##מה זה ולמה עושים את זה?
YAML הוא פורמט נתונים פשוט וקריא שמשמש בעיקר לעבודה עם קבצי תצורה (configuration) ונתונים מעוצבים (structured data). מפתחים ומפתחות משתמשים בו בכדי לארגן ולנהל נתונים בצורה ברורה וקלה לקריאה ולכתיבה.

##כיצד לכתוב קוד:
קוד דוגמה ותוצאות אפשריות ב־TypeScript ניתן לראות בתרגיל הקוד המצורף בהמשך:

```TypeScript
// הגדרת משתנה שיופיע כיותר מפעם אחת
const person = { name: 'John', age: 30, profession: 'developer' };

// הוספה של נתונים נוספים למשתנה
person.country = 'USA';

// כתיבת הנתונים מכתובת משתנה לתבנית YAML
const output = `
name: ${ person.name }
age: ${ person.age }
profession: ${ person.profession }
country: ${ person.country }
`;

// הדפסת התבנית
console.log(output);
```

תוצאה:
```
name: John 
age: 30 
profession: developer 
country: USA
```

##עיון מעמיק:
YAML נוצר לראשונה על ידי מתכנתים של חברת תוכנה ומכיל רעיונות מכל מיני פורמטים קיימים כגון JSON, XML ו־Cמתי. אלטרנטיבות של YAML כוללות פורמטים אחרים שייתכן ותתאימו לא יותר מסוג־מסוג לפרוייקטים ספציפיים. למידע נוסף וביצועים נא לבדוק את המקורות המצורפים למטה.

##ראו גם:
- [The official YAML website](https://yaml.org/)
- [A Beginner's Guide to YAML by DigitalOcean](https://www.digitalocean.com/community/tutorials/an-introduction-to-yaml)