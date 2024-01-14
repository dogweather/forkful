---
title:                "TypeScript: עובדים עם JSON"
simple_title:         "עובדים עם JSON"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## למה

התכונן לעבוד עם JSON יכול להיות טוב לך כמתכנת. נתחיל!

## איך לעשות

אנו נפתח עם דוגמא פשוטה של קובץ JSON:

```TypeScript
const person = {
  name: "John Smith",
  age: 30,
  hobby: "reading"
};

console.log(person.name); // Output: John Smith
```

כאן, אנו משתמשים בקובץ JSON על מנת לאחסן מידע על אדם כגון שם, גיל ותחביב. נגיע למשתנים בתוך הקובץ באמצעות הסימן "." ומדפיסים אותם בעזרת הפונקציה "console.log". כמו כן, ניתן לעשות שימוש במערך של עצמים בקובץ JSON:

```TypeScript
const students = [
  {
    firstName: "Michael",
    lastName: "Brown",
    age: 24
  },
  {
    firstName: "Samantha",
    lastName: "Green",
    age: 22
  },
  {
    firstName: "David",
    lastName: "Taylor",
    age: 26
  }
];

console.log(students[1].lastName); // Output: Green
```

כאן, אנו משתמשים במערך של עצמים על מנת לאחסן מידע של מספר סטודנטים. אנו נגיע לאחד מהם באמצעות הערך המספרי במערך ונדפיס את מחרוזת השם המשפחה.

## מעמקים

כאשר אנו עובדים עם JSON, חשוב לזכור כי זהו פורמט נתונים שמשמש בדרך כלל להעביר מידע בין שרתים ולהוציא מידע ממסדי נתונים. פורמט זה מתאים לשימוש בכל מספר שפות תכנות כולל TypeScript.

עוד דבר שחשוב לזכור הוא שקבצי JSON צריכים להיות חוקיים מבחינת התחביר, כלומר לעקוב אחר כללי התחביר של הפורמט ולשמור על נכונות ועדכניות של המידע.

## ראה גם

* מדריך לפורמט JSON - https://www.json.org/json-he.html
* היכרות עם קובץ JSON ב-TypeScript - https://www.typescriptlang.org/docs/handbook/interfaces.html#introduction