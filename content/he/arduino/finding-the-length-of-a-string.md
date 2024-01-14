---
title:    "Arduino: מציאת אורך של מחרוזת"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

Why (למה): ברור שמצאת לעצמך מצבים בהם היית רוצה לדעת את האורך של מחרוזת מסוימת, כגון כאשר אתה עובד עם גרפיקות או על מסך הלד. לכן, לדעת את אורך המחרוזת חשוב כדי להתאים בצורה נכונה את התצוגה שלך.

How To (כיצד לעשות): בשביל למצוא את האורך של מחרוזת בארדונו, אתה צריך להשתמש בפקודת "length()" ולהעביר לה את המחרוזת המבוקשת. ניתן לעשות זאת באמצעות פקודת "Serial.print()" כדי להדפיס את התוצאה של האורך. להלן דוגמא:

```Arduino 
String myString = "מחרוזת נסתרת";
int length = myString.length(); // מציג את אורך המחרוזת (10)
Serial.print(length); // הדפסת התוצאה (10)
```

Deep Dive (טביעה עמוקה): למרבה המזל, בארדונו אין צורך לציין את אורך המחרוזת בפקודה "length()", אלא ניתן להשתמש בפקודת "strlen()" כדי לקבל את התוצאה ישירות. השימוש בפקודה זו יכול לתרום למהירות ולפשטות הקוד. לדוגמא:

```Arduino
String myString = "מחרוזת נסתרת";
int length = strlen(myString); // מציג את אורך המחרוזת (10)
Serial.print(length); // הדפסת התוצאה (10)
```

See Also (לראות גם): 
- [מדריך על שימוש במחרוזות בארדונו](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [כיצד למצוא את התו הראשון ממחרוזת בארדונו](https://www.arduinocentral.net/technical-discussions/how-to-find-the-first-character-of-a-string-in-arduino/)