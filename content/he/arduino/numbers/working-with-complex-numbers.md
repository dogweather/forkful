---
title:                "עבודה עם מספרים מרוכבים"
aliases: - /he/arduino/working-with-complex-numbers.md
date:                  2024-01-26T04:37:24.821206-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים מורכבים מחלק ממשי וחלק מדומה, בדרך כלל נכתבים כ- `a + bi`. הם חיוניים לפרויקטים של ארדואינו בעלי עומס מתמטי הקשורים לעיבוד סיגנלים, הנדסה חשמלית, או כל תחום אחר בו התופעות מודגמות בצורה הטובה ביותר במישור.

## איך לעשות:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // התחלת תקשורת סריאלית
  
  Complex myComplex(2, 3); // יצירת מספר מרוכב 2 + 3i
  Complex anotherComplex(1, 1); // יצירת מספר מרוכב נוסף 1 + 1i
  
  // חיבור
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addition: "); 
  result.print(); // מוציא 3 + 4i
  
  // כפל
  result = myComplex * anotherComplex; 
  Serial.print("Multiplication: ");
  result.print(); // מוציא -1 + 5i
}

void loop() {
  // לא בשימוש בדוגמה זו
}
```
תצוגת דוגמה:
```
Addition: 3 + 4i
Multiplication: -1 + 5i
```

## צלילה עמוקה
מקורית, התייחסו למספרים מרוכבים בחשדנות, אך הם הפכו להיות מרכזיים במגוון שדות מדעיים. בהיסטוריה, הם התקבלו בזכות הספקת פתרונות למשוואות פולינומיות שאין להן פתרונות ממשיים.

ארדואינו לא כולל מספרים מרוכבים בספריית הסטנדרט שלו, אך אתה יכול להשתמש בספריות כמו `Complex.h` לטיפול בהם. באופן פנימי, ספריות אלו מגדירות מחלקת Complex, בדרך כלל באמצעות שימוש בשני משתנים מסוג double לאחסון החלקים הממשי והמדומה, ומעמיסות אופרטורים לתמיכה באריתמטיקה.

כחלופה, ליישומים שאינם דורשים באופן עקרוני אריתמטיקה של מספרים מרוכבים, שקלו להשתמש באסטרטגיות מתמטיות אחרות או בספריות אחרות. זכור, עם זאת, ששימוש במספרים מצומדים במקום במספרים מרוכבים עלול לפשט יתר על המידה חלק מהבעיות.

## ראה גם
- הספרייה [Complex.h](https://github.com/RobTillaart/Complex) מאת רוב טילארט.
- צלילה עמוקה יותר לתוך [המתמטיקה מאחורי מספרים מרוכבים](https://mathworld.wolfram.com/ComplexNumber.html).
