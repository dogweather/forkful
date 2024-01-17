---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "TypeScript: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

למה ומדוע?

מחיקת תווים המתאימים לתבנית היא פעולה נפוצה בתכנות, המשמשת למחיקת תווים ספציפיים מהטקסט בהתאם לתבנית מסוימת. תהליך זה נעשה כדי להקל על המילוי ועיבוד הנתונים בקוד ובאיכותו של התוצאה הסופית.

כיצד לבצע:

ב TypeScript ניתן למחוק תווים המתאימים לתבנית באמצעות פונקציות מובנות כמו ```replace()``` ו- ```replaceWith()```. לדוגמה, אם נרצה למחוק את האות "a" מתוך המחרוזת "banana", נוכל להשתמש בקוד הבא:

```
let str = "banana";
let newStr = str.replace(/a/g, "");
console.log(newStr); // bnn
```

במקרים מסוימים, כגון מחיקת מערכת הפנקטואציה מהקלט, ניתן להשתמש בשיטות יעילות יותר כגון ```split()```, ```join()``` ו- ```map()``` על מנת למחוק תווים לפי תבנית מסוימת.

עיון מעמיק:

מחיקת תווים המתאימים לתבנית התחשבלה לאופן ההתפתחות של שפת התכנות JavaScript מאז הושקה בשנת 1995. מאז התחלקה שפה זו למספר תתי שפות, כולל TypeScript שהושקה בשנת 2012 על ידי חברת מיקרוסופט. מאז, שפת TypeScript הפכה לפופולארית יותר בקרב מתכנתים לכן יכול כעת למחוק תווים המתאימים לתבנית התחשבלה.

אגודת מילה מכילה מילה אחת או יותר המהווה ביטוי נודלאי הפועל בתוך הקוד. פעולות נוספות למחיקת תווים מוכרות כוללות פונקציות כמו ```substr()```, ```slice()```, ו- ```splice()``` ו שינוי של כוח הפעולה הו -icon.

תראה גם:

למידע נוסף על תרגילים נרקיסאים ב TypeScript: https://www.geeksforgeeks.org/delete-all-occurrences-of-a-given-character-in-a-string-using-regex-in-typescript/.

בנוסף, ניתן למצוא פרסומים מענינים נוספים על מחיקת תווים מתאימים לתבנית ב TypeScript כאן: https://www.codewall.co.