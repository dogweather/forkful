---
title:                "המרת מחרוזת לאותיות קטנות"
aliases: - /he/google-apps-script/converting-a-string-to-lower-case.md
date:                  2024-02-01T21:52:18.664534-07:00
model:                 gpt-4-0125-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

המרת מחרוזת לאותיות קטנות ב-Google Apps Script, שפת סקריפט מבוססת ענן לאוטומציה של משימות במוצרי Google, היא משימה יסודית המכוונת לתקן נתוני טקסט. תכנתים לעיתים קרובות מבצעים פעולה זו כדי להבטיח עקביות בקלט של המשתמש, בעיבוד נתונים, או בהשוואה בין מחרוזות, מכיוון שזה מסיר בעיות של רגישות לאותיות גדולות וקטנות.

## איך לעשות:

המרת string לאותיות קטנות ב-Google Apps Script היא פשוטה להפליא, בזכות השיטות JavaScript המובנות הזמינות בסביבת הסקריפט. השיטה `toLowerCase()` היא זו שתבחרו להשתמש בה בעיקר. הנה איך תוכלו ליישם אותה:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // מוצג: !hello, world
}
```

פונקציה פשוטה זו מדגימה לקיחת string מקורי, החלת השיטה `toLowerCase()`, ורישום התוצאה. זה במיוחד שימושי כאשר מתמודדים עם קלטים שצריכים להיות לא רגישים לאותיות קטנות וגדולות. לדוגמה, בהשוואת כתובות דוא"ל שמשתמשים עשויים להזין במקרים שונים.

בנוסף, עבור מצבים בהן אתם עובדים עם נתוני מערך, תוכלו לעבור דרך כל אלמנט כדי להמיר אותם לאותיות קטנות:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // מוצג: [alice, bob, charlie]
}
```

דוגמה זו מדגישה את גמישותה של `toLowerCase()` בעת טיפול במרובי נתוני string, ומבטיחה אחידות ברחבי קבוצת הנתונים שלכם.

## חקירה עמוקה

השיטה `toLowerCase()`, שנרשתה מ-JavaScript ומשמשת בתוך Google Apps Script, הייתה חלק בלתי נפרד מניפוי strings מאז הגרסאות הראשונות של JavaScript. המטרה העיקרית שלה היא לעזור בטיפול לא רגיש לאותיות גדולות וקטנות בנתוני טקסט, צורך שעלה עם בואם של אפליקציות אינטרנט דינמיות שמספקות אינטרקציה עם המשתמש. למרות פשטותה, המנגנון ממלא תפקיד חשוב באימות נתונים, מיון ואלגוריתמים של חיפוש על ידי הפחתת המורכבות הנכנסת על ידי רגישות לאותיות גדולות וקטנות.

מבחינת ביצועים, תהליך ההמרה מאוד מיועל במנועי JavaScript מודרניים; עם זאת, ישנה עדיין חשיבות ליישום זהיר בתוך פעולות נתונים בקנה מידה גדול כדי להימנע מעומס עיבוד לא נחוץ.

חלופה שכדאי לשקול, בעיקר כאשר עובדים עם דפוסים מורכבים או שצריך המרות ספציפיות לאזור, היא השיטה `toLocaleLowerCase()`. גרסה זו לוקחת בחשבון כללים ספציפיים לאזור להמרת תווים לאותיות קטנות, מה שעשוי להיות חיוני עבור אפליקציות התומכות במספר שפות:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // מוצג: märz
```

למרות המורכבות הנוספת, `toLocaleLowerCase()` היא כלי עוצמתי עבור אפליקציות בינלאומיות, מבטיחה שההמרה תכבד את הנורמות הלשוניות של אזור המשתמש. בין אם אתם בוחרים בשיטה הזו או באחרת, המרת strings לאותיות קטנות נשארת חלק בלתי נפרד מעיבוד טקסט ב-Google Apps Script, גורמת לגשר בין קלט משתמש לבין טיפול נתונים מתוקנן.
