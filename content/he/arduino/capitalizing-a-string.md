---
title:                "Arduino: ניצור נושא למאמר 'כיצור מחרוזת' על תכנות מחשב."
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

הכתיבה באותיות ראשונות משנה לסקירור מחרוזות וייצוג יפה יותר של נתונים במכשירי ארדואינו. זה יכול להיות שימושי כאשר אנחנו רוצים להציג טקסט במסך LCD, לשלוח מידע דרך יציאת סיריאלית או פשוט נרצה להפיק את השם של משתמש מכתובת אימייל.

## איך לעשות זאת

בכדי לכתוב את הקוד למכשיר ארדואינו נשתמש בפונקצייה ```capitalize```. לדוגמה, אם נרצה לכתוב את המחרוזת "hello world" על המסך LCD, נשתמש בקוד הבא:

```arduino
#include <LiquidCrystal.h> 

LiquidCrystal lcd(12, 11, 5, 4, 3, 2);

void setup() {

  lcd.begin(16, 2);
  lcd.print(capitalize("hello world")); // אנחנו קוראים לפונקציית capitalize כאן

}

void loop() {

  // כאן אנחנו ממתינים לקליטה נוספת

}
```

כתוצאה מכך, נקבל את התוצאה הבאה על המסך:

```
Hello world
```

## צעדים עמוקים

כאשר אנחנו קוראים לפונקציית ```capitalize```, אנחנו מעבירים לה את המחרוזת שאנחנו רוצים לסקור, כך:

```arduino
capitalize("hello world");
```

פונקציית ```capitalize``` תחזיר את המחרוזת עם האות הראשונה בכתיב ראשוני ואת שאר האותיות בכתיב קטני. אם ננסה לכתוב:

```arduino
capitalize("hELlO wORLD");
```

נקבל כתוצאה:

```
Hello world
```

כמו כן, אם ננסה לכתוב מספרים או תווים מיוחדים בתוך המחרוזת, הם יישמרו בכתיב המקורי. לדוגמה:

```arduino
capitalize("h3llo w0rld!");
```

תחזיר:

```
H3llo w0rld!
```

## ראה גם

למאמרים נוספים בנושא תכנות בפלטפורמת ארדואינו, ראה את הקישורים הבאים:

- [התחלה עם ארדואינו: מדר