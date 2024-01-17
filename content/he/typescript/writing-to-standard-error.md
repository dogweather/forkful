---
title:                "כתיבה לשגיאת התקן"
html_title:           "TypeScript: כתיבה לשגיאת התקן"
simple_title:         "כתיבה לשגיאת התקן"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

מה ולמה?
כתיבה לשגיאות סטנדרטיות היא כלי חשוב בתכנות שמשמש להדפסת הודעות שגיאה ומידע בזמן ריצה. היא מאפשרת למפתחים לאתר בעיות בקוד ולהגיב להן במהירות וביעילות. כתיבה לשגיאות סטנדרטיות היא גם דרך להוסיף מידע נוסף על פעולת התוכנית ולהבהיר את תהליך הריצה עבור מפתחים ומשתמשים חיצוניים.

איך לעשות?
ב TypeScript כתיבה לשגיאות סטנדרטיות מתבצעת על ידי השתמשות בפונקציית 'console.error()'. הערך שנמסר לפונקציה יודפס כהודעת שגיאה בסביבת הריצה. לדוגמה, אם נרצה להדפיס הודעת שגיאה עבור טיפוס לא תקין, נריץ את הפונקציה הבאה:

```TypeScript
function validateAge(age: number) {
  if (age < 0) {
    console.error("Invalid age!");
  }
}
```

כתוצאה מכך, כל עת שיופעלה הפונקציה 'validateAge()', יתווסף הודעת שגיאה לסביבת הריצה עם המידע הרלוונטי.

דגימת פלט
לפני:
```TypeScript
function calculateCircleArea(radius: number) {
  return 3.14 * radius * radius;
}

let area = calculateCircleArea(-5);
```

אחרי:
```TypeScript
function calculateCircleArea(radius: number) {
  if (radius < 0) {
    console.error("Invalid radius!");
  }
  return 3.14 * radius * radius;
}

let area = calculateCircleArea(-5);
```

עומק נרחב
בעבר, לפני התקנויות הדפסה סטנדרטיות, מפתחים היו נאלצים להשתמש בדרכים נוספות כדי לאתר ולטפל בשגיאות בזמן ריצה. הדרך הקלאסית הייתה בשימוש בפונקצייה 'console.log()' ולהדפיס את הודעות השגיאה באמצעות קוד מתזה. אך בעזרת כתיבה לשגיאות סטנדרטיות, התהליך הפך להיות יותר קל ונוח עבור מפתחים וקוראים חיצוניים.

למרבה המזל, ישנן אפשרויות נוספות להדפסת שגיאות ב TypeScript, כגון השתמשות בספריית צד שלישי כמו 'node-logger' או בבניית מנגנון מותאם אישית. אך כתיבה לשגיאות סטנדרטיות היא עדיין הדרך המומלצת והנפוצה ביותר עבור מפתחים.

ראו גם
- [דוקומנטציה של TypeScript על כתיבה לשגיאות סטנדרטיות](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#write-only-data-errors-to-standard-error)
- [מדריך מפורט על כתיבה לשגיאות ב TypeScript](https://jstobigdata.com/ts/typescript-console-error-write-to-standard-error/)