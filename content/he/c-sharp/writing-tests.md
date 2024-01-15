---
title:                "כתיבת בדיקות"
html_title:           "C#: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# מדוע

למה שום אחד לא אוהב לכתוב בדיקות. זה נוצר גורם לנו להעליש את הכלים שלנו בשביל להיות מזמינים יותר ולעבוד יותר מהר. אבל אנחנו יודעים שזה לא הדבר הנכון לעשות. אז למה עדיין יש מפתחים שמסרבים לכתוב בדיקות? איך אנחנו יכולים לשנות את זה ולהתחיל לכתוב בדיקות בצורה יעילה? בואו נמצא את התשובה באמצעות C#.

# איך לעשות זאת

כדי להתחיל לכתוב בדיקות, נצטרך להשתמש בכלים שלמדנו ולשדרג אותם כדי שהם יתאימו לכתיבת בדיקות. הנה כמה דוגמאות קוד כדי להתחיל:

```c#
// ייצור מחלקה חדשה

public class Calculator 
{
    public int Add(int x, int y) 
    {
        return x + y;
    }
}

// כעת ניצור מחלקת בדיקות עבור המחשבון

public class CalculatorTests
{
    [Fact]
    public void Add_ShouldReturnCorrectSum() 
    {
        // הכנסת מספרים לבדיקה
        int x = 5;
        int y = 10;

        // יצירת מחשבון חדש על מנת לבדוק את הפונקציה Add
        Calculator calculator =new Calculator();

        // הפעלת הפונקציה Add ומציאת התוצאה הנכונה
        int expected = 15;
        int actual = calculator.Add(x, y);

        // אמת אם התוצאה נכונה
        Assert.Equal(expected, actual);
    }
}
```

## Deep Dive

אחרי ששכללנו את הכלים שלנו ולמדנו איך לכתוב בדיקות יעילות ב-C#, טען זה שיהיה נבישות לקבוצת העבודה. אבל אולי ישנם יתרונות לכתיבת בדיקות שטרם הכרתם. למשל, בדיקות מעלים את האמונה שלנו בקוד שלנו ומפחיתים את הסיכונים בעת השינויים והתיקון של בעיות. הם יכולים גם לסייע בטיפול בבעיות באמצעות מציאת המקור לבאגים ולהמנע מ