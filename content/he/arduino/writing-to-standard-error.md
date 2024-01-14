---
title:    "Arduino: כתיבה לטעות תקן במחשב"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה:

כתיבה לשגיאות סטנדרטיות (standard error) היא חלק לא נפרד מתכנות ארדוינו. בכתיבה לשגיאות סטנדרטיות ניתן לטפל בשגיאות ולמצוא בעיות בקוד בצורה יעילה ומתוחכמת.

## איך לעשות זאת:

הדוגמה הבאה מציגה כיצד לכתוב לשגיאות סטנדרטיות בתוך קוד ארדוינו מתוך הסברים מפורטים.

```Arduino
#include <avr/pgmspace.h> 
#include <avr/io.h>

void setup() {
  Serial.begin(9600); // פתיחת חיבור סיריאלי
  stderr = &Serial; // הגדרת כתיבה לשגיאות סטנדרטיות לחיבור סיריאלי
  Serial.println("Hello World!"); // כתיבת הטקסט "Hello World!" לחיבור סיריאלי
}
```
התוצאה של הדוגמה הנ"ל תהיה: `Hello World!`

ניתן גם להדפיס משתנים לשגיאות סטנדרטיות, לדוגמה:

```Arduino
#include <avr/pgmspace.h> 
#include <avr/io.h>

void setup() {
  Serial.begin(9600); // פתיחת חיבור סיריאלי
  stderr = &Serial; // הגדרת כתיבה לשגיאות סטנדרטיות לחיבור סיריאלי
  int num = 5; // הגדרת משתנה עם ערך 5
  Serial.println(num); // כתיבת המשתנה לחיבור סיריאלי
  Serial.println("The value of num is:"); // כתיבת מסר לחיבור סיריאלי
}
```

התוצאה של הדוגמה הנ"ל תהיה:

```
5
The value of num is:
```

## דיב מעמוק:

כתיבה לשגיאות סטנדרטיות היא חשובה כאשר מעוניינים לעקוב אחר מקרי שגיאה ולטפל בהם בצורה מהירה ויעילה. היא מאפשרת למצוא בעיות בקוד ללא הפסקת התכנות וסיוע בזיהוי המיקום המדויק של השגיאה. כמו כן, זיהוי מספר המקריים של השגיאה יכול לעזור בזיהוי נקודות חלשות בקוד ושיפור