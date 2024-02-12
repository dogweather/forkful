---
title:                "עבודה עם מספרים מרוכבים"
aliases: - /he/java/working-with-complex-numbers.md
date:                  2024-01-26T04:42:36.461984-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

מספרים מרוכבים מרחיבים את קו המספרים הממשיים באמצעות הוספת יחידה מדומה, `i`, כאשר `i^2 = -1`. הם חיוניים בתחומים כמו הנדסה, פיזיקה, ומתמטיקה מתקדמת, שם הם ממדלים תופעות שמספרים ממשיים לא יכולים להתמודד איתם, כמו זרמים חשמליים ועיבוד אותות.

## איך לעשות:

הג'אווה אינה תומכת מובנית במספרים מרוכבים, אך אנו יכולים לפתח מחלקה משלנו או להשתמש בספרייה. הנה דוגמה מהירה ליצירת מחלקת `ComplexNumber` פשוטה ושימוש בה:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString להצגת מספרים מרוכבים בפורמט a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // בדיקה מהירה
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("סכום: " + c1.add(c2));
    }
}
```

פלט לדוגמה לשיטת ה-main יהיה:

```
סכום: 3.0 + 7.0i
```

## חקר עמוק יותר

לפני שפות גבוהות כמו ג'אווה, מתכנתים עבדו ישירות עם ספריות מתמטיות בשפות כמו פורטרן או C לניהול של פעולות מורכבות. הרעיון חוזר אחורה למאה ה-16, שמיוחס למתמטיקאים כמו ג'רולמו קרדנו ורפאל בומבלי.

בג'אווה, `java.lang.Math` היא נקודת פניה לעניינים תכליתיים אך מדלגת על מספרים מרוכבים, כנראה משום שלא כל מתכנת משתמש בהם. אלטרנטיבות? שימוש בספריות. Apache Commons Math מספקת מחלקת `Complex` עמוסה בשיטות למניפולציה. הנה למה לפתח בעצמך זה מגניב: מתוחכם, מותאם אישית בדיוק לצרכים שלך, וללא עלות של ספרייה.

פרט חשוב אחד: שימו לב לדיוק של נקודה צפה. מחשבים לא יכולים לייצג כמה מספרים בדיוק, מה שמוביל לשגיאות עיגול. בעת ביצוע של פעולות מורכבות חוזרות ונשנות, שגיאות אלו יכולות להצטבר!

## ראו גם

לחקירה עמוקה יותר ופעולות מורכבות נוספות, עיינו ב:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [מחלקת Complex של JScience](http://jscience.org/)
- הדרכות של Oracle על [חישוב בנקודה צפה](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
