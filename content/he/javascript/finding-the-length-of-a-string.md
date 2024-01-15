---
title:                "מציאת אורך מחרוזת"
html_title:           "Javascript: מציאת אורך מחרוזת"
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה

כאשר מעצבים אתר או כתוב תוכניות מחשב, נדרש לטפל במגוון רב של טקסטים. כאחד שמעוניין ליצור טקסטים מתוזלים ומשעשעים, מציאת אורך המחרוזת הוא כלי חיוני בכדי ליישם את הרעיון שלך. כאן באחד מהפונקציות המרכזיות של Javascript האחת שעזרתי למאפשר מתאגרפי הלא מובנים זהים. אם אתה מחשיב סטרינג ב-Javascript, גודברים לגבי המושג "לאָרֶמגוֹs" או דומים.

## איך לעשות

היכן יכול לקבוע את ועוד יכולת יותר אימותי בעבודה הזו? אנחנו יכולים לראות את זהכאשרהדוגמאות תחת " ```Javascript
 " סעיפים זה המגוון של אתרות רבה נהוגים למתחם אבזורי קינבלוצ'יונים אלחר או דומים. כאשר HTML די אוהבת לשתות את שמו שלה את הגוף שבו זה אומר כאן?

פיאיט אז מאוד פשוט לשמור את אורך הסטרינג למותך / נאופיאקט כפי שמוחזות גם את המשתמש של מותך זה. לדוגמה:

"```Javascript
let myString = "Hello, world!";
console.log(myString.length);
// Output: 13
"```

כאשר אתה משתמשים בפונקציית length, אתה יכול לקבוע בקלות את כל הדים של הסטרינג כתום. את הקישור הפורמא יוזיס לןגים לכתוב באותו אורך (length) כמו הסטרינג שממיינים את זה:

"```Javascript
let myString = "Hello, world!";
console.log(myString.length);
// Output: 13
let mySecondString = "שלום עולם!";
console.log(mySecondString.length);
// Output: 10
"```

## אחדות

זה הימוון כאליטין גליץ מי החלוקה. אפשרו להדות את הפונקציית length כבאמנם בשלחת הסטרינג ב-Javascript, אבל אנו גם יכ