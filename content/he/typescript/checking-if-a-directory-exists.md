---
title:    "TypeScript: בדיקת קיום תיקייה"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מדוע

בדיקת האם תיקייה קיימת יכולה להיות חשובה בכתיבת קוד ב-Typescript. למשל, כאשר יש צורך למעדכן או ליצור תיקייה לפני שביצוע פעולה מסוימת.

## איך לעשות

בתחילת הקוד, נצרף את המודול fs של Node.js כדי לאפשר לנו לגשת לפעולות הקשורות לקבצים ותיקיות. לאחר מכן נשתמש בפעולת fs.existsSync() כדי לבדוק האם התיקייה קיימת או לא.

```TypeScript
import * as fs from 'fs';
const dirPath = './myDirectory';
if (fs.existsSync(dirPath)) {
	console.log('התיקייה קיימת!');
} else {
	console.log('התיקייה לא קיימת.');
}
```

תוצאה עבור הקוד הנ"ל לאחר ריצה:

```
התיקייה לא קיימת.
```

## ירידה עמוקה

פעולת fs.existsSync() בודקת האם התיקייה קיימת ישירות מקובץ ה-JSON שמייצג אותה. במילים אחרות, היא בודקת את הנתיב המוגדר במשתנה dirPath ולא בודקת אם נוכל לגשת אליו. במצב כזה, אם הנתיב אינו קיים, הפקודה תחזיר false בכל מקרה. זה יכול לגרום לבעיה אם נעשה שימוש בפעולה זו על תיקייה שנמצאת בתיקייה שאין לנו הרשאות לגשת אליה. לכן, עלינו לוודא שאנו מעדיפים לבדוק אם הנתיב המוגדר במשתנה קיים על ידי השתמשות בפעולה fs.accessSync() עם הפרמטר fs.constants.F_OK.

## ראה גם
- [מודול fs של Node.js](https://nodejs.org/api/fs.html)
- [הסברים נוספים על בדיקת קיום של תיקיות ב-Typescript](https://www.tutorialspoint.com/typescript/typescript_checking_a_directory.htm)