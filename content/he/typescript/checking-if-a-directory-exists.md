---
title:    "TypeScript: בדיקת קיומו של תיקייה בתוכנית מחשב"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## למה


בעולם התכנות, ייצור וניהול ספריות וקבצים הוא חלק חשוב מתהליך הפיתוח. בכדי להבטיח שהיישומים שלנו פועלים כהולכים ושלא מתרחשות דפוקוות לא צפויות, עלינו לוודא שהקבצים והתיקיות המבניים של היישומים קיימים ומסודרים. אחת המשימות החשובות שבין היתר אנו מקיימים היא בדיקת קיומו של תיקייה כלשהי.

## איך לבדוק אם תיקייה קיימת ב TypeScript

בשפת TypeScript יש לנו פונקציה פשוטה המאפשרת לנו לבדוק אם תיקייה קיימת במערכת הקבצים. כדי להשתמש בפונקציה זו, עלינו לייצר משתנה מסוג `fs` שמייצג את הספריה המשמשת לניהול קבצים. לאחר מכן, נשתמש בפונקציה `existsSync()` כדי לבדוק את קיומו של התיקייה.

```TypeScript
import fs from 'fs';

const directory = './myFolder';

if (fs.existsSync(directory)) {
  console.log('התיקייה קיימת!');
} else {
  console.log('התיקייה לא קיימת :(');
}
```

בקוד זה אנו יוצרים משתנה נקודתי `directory` שמכיל את הנתיב לתיקייה המבוקשת. באמצעות פונקצית `existsSync()` אנו בודקים אם תיקייה עם הנתיב הזה קיימת במערכת הקבצים. במקרה שהתיקייה קיימת, הדפסנו הודעה מתאימה. אם התיקייה לא קיימת, נדפיס הודעת שגיאה.

## לחקור עמוק יותר

כדי להבין את הפונקציה `existsSync()` טוב יותר, נדגים אותה בתוך תוכנית קטנה שבה אנו יצרים תיקייה חדשה ובודקים את קיומה.

```TypeScript
import fs from 'fs';

const directory = './myNewFolder';

if (fs.existsSync(directory)) {
  console.log('התיקייה קיימת!');
} else {
  // יצירת תיק