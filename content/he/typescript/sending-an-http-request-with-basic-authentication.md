---
title:                "TypeScript: שיגור בקשת http עם אימות בסיסי בתכנות מחשבים"
simple_title:         "שיגור בקשת http עם אימות בסיסי בתכנות מחשבים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

בעולם המתפתח של היום, יישומי האינטרנט הפכו לחלק בלתי נפרד מחיינו. יישום אינטרנט מתקשר עם שרת כדי לקבל נתונים או לבצע פעולות. כדי להבטיח שמישהו קשור אכן למערכת שלך, קיימת טכניקת אימות ייחודית, התרחיש הנפוץ ביותר הוא האימות בסיסי. בכתב הטכנולוגית הזה אני אעזור לכם להבין איך לשלוח בקשת HTTP עם אימות בסיסי ב TypeScript.

## איך לעשות זאת

```typescript
import axios from 'axios';

const username = 'username';
const password = 'password';

axios.get('https://example.com', {
  auth: {
    username,
    password
  }
})
.then(response => {
  console.log(response);
})
.catch(error => {
  console.log(error);
});

```

כדי לשלוח בקשת HTTP עם אימות בסיסי ב TypeScript, אנו נעשה שימוש בספרייה שנקראת Axios. תחילה, אנו מייבאים את הספרייה ומגדירים את שם המשתמש והסיסמה. לאחר מכן, אנו משתמשים בפונקציה "הבא" כדי לשלוח את הבקשה ולקבל תגובה מהשרת. אם יש שגיאה, אנו משתמשים בפונקציה "תפוס" כדי להדפיס את השגיאה.

בגלל שאנחנו משתמשים באימות בסיסי, אנחנו חייבים לספק את השם והסיסמה בבקשה. כך, בשורת הצוותים של הבקשה, אנחנו משתמשים בערך "כתובת אתר". תחת האובייקט "אמות", אנו מצהירים את השם והסיסמה. בסופו של דבר, אנחנו מקבלים את התגובה ומדפיסים אותה לקונסולה.

## כיוון מקדים

בקשות HTTP עם אימות בסיסי משמשות גם ליישומים אחרים. לדוגמה, בעת בקשה למידע ממסד נתונים שיש בו מידע רגיש, עלינו לאמת את האישור שנמ