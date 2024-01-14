---
title:    "Arduino: מחיקת תווים התואמים לתבנית."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

פעמים רבות הצורך במיקור חשבונות או מחיקת תווים עם תבנית מסוימת יכול להיות כבד וקשה ליישום. בעזרת קוד Arduino, אפשר לפשט וליישם בצורה יעילה את צעדי המחיקה של תווים בהתאם לתבנית שנבחרה.

## כיצד לישום

באמצעות פונקציות המחיקה הקיימות בלוח Arduino, אפשר להסיר תווים תואמים לתבנית מסוימת מסדרת תווים. נביא לדוגמה כמה קודים לארדואינו כדי להמחיש את התהליך.

```Arduino
// מציאת תווים תואמים לתבנית ומחיקתם מסדרת התווים הנתונה
String input = "ABCD1234"; // קלט נתון
String pattern = "CD"; // תבנית למיקור חשבונות
while (input.indexOf(pattern) > -1) { // בדיקה אם תווים תואמים נמצאים בסדרת התווים
  int pos = input.indexOf(pattern); // מיקום התווים התואמים
  input.remove(pos, pattern.length()); // מחיקת התווים הנמצאים במיקום הנתון על פי התבנית
}
Serial.println(input); // פלט: AB1234
```

## כיווץ מקיפה

מחיקת תווים תואמים לתבנית מתבצעת על ידי חיפוש התבנית בסדרת התווים ומחיקתם בהתאם. אם התבנית מופיעה יותר מפעם אחת בסדרה, הפעולה תיכפל ותמחק כמה תווים כמה פעמים כפול מחיקת התבנית. בנוסף, קיימת אפשרות להסתיר את התווים המוחקים על ידי הדבקת לבן את כל התווים שנמענים בקנה אחד לאחר כל המחיקה.

## ראה גם

למידע נוסף על קוד ארדואינו ותפקידו בעולם התכנות הפיזי, היכנסו לקישורים המשמיעים להלן:
https://www.arduino.cc/reference/en/language/functions/string-functions/removesymbolsmatchingpattern/
https://programminghistorian.org/en/lessons/physical-com