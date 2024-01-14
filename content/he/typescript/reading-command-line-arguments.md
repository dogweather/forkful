---
title:    "TypeScript: קריאת ארגומנטים מפניית הפקודה"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה
קריאת פרמטרי שורת הפקודה היא כלי עוצמתי לפיתוח תוכניות בטיפול בשורת הפקודה. זהו דרך קלה ויעילה לתקשר עם המשתמש ולקבל קלט משתמש בזמן אמת.

## איך לעשות זאת
נדגים את דרך הקריאה והשימוש בפרמטרי שורת הפקודה ב TypeScript, באמצעות שוואו קוד ופלט דוגמה שנמצאים בחסות שורת הפקודה.

```TypeScript
// כדי לקרוא פרמטרי שורת הפקודה, נשתמש במשתנה process.argv המכיל את כל הפרמטרים הנמצאים בשורת הפקודה
let argv = process.argv;

// נוכל לאתר ולקרוא את הפרמטרים על ידי אינדקס כגון argv[0], argv[1] וכו'
// אינדקס 0 יהיה תמיד הנתיב לקובץ האקסקיוטבל של התוכנית
// אינדקס 1 יהיה תמיד הפרמטר הראשון שנעבוד איתו
// לדוגמה:
console.log(argv[0]); // output: 'node'
console.log(argv[1]); // output: 'app.js'
console.log(argv[2]); // output: 'arg1'
console.log(argv[3]); // output: 'arg2'
console.log(argv[4]); // output: 'arg3'

// נוכל להשתמש גם בפקודת for לבדיקת כל הפרמטרים בצורה אוטומטית
for (let i = 0; i < argv.length; i++) {
  console.log(argv[i]); // output: איפה לקבל אותם וכיוון שכל פרמטר תתקבל כאגרגט, הכולל גם רווחים
}

// עכשיו נכין פונקציה שתכיל את כל המדריכים לפרמטרי שורת הפקודה בצורה ברוחב ובגובה שלהם כדי להפעיל את התוכנית בקוד
function printHelp() {
  console.log('Usage: node app.js <command> [<args>]');
  console.log('Options:');
  console.log('    -h, --help         Output usage information');
  console.log('    -v, --version      Output the version number');
}

// אז תיהיה כמו כן ליצור פונקצית switch לפרמטרי שורת הפקודה
// כדי להעב