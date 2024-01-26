---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת טסטים היא בדיקת הקוד שלך באמצעות סט נסיונות שונים כדי לוודא שהוא עובד כראוי. מתכנתים עושים זאת כדי לזהות באגים, למנוע קריסות ולשפר את איכות הקוד לפני השחרור לייצור.

## איך לעשות:

```C#
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace MyApp.Tests
{
    [TestClass]
    public class CalculatorTests
    {
        [TestMethod]
        public void Add_Values_ReturnsSum()
        {
            // כאן אנו יוצרים אינסטנס של המחלקה שאנחנו רוצים לבדוק
            Calculator calc = new Calculator();

            // כעת בודקים את פעולת החיבור
            int result = calc.Add(5, 7);
            
            // כאן אנחנו צופים שהתוצאה תהיה 12
            Assert.AreEqual(12, result);
        }
    }
    
    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}
```
הפלט במקרה הנ"ל יהיה הודעת עבור טסט שעבר בהצלחה או נכשל.

## צלילה עמוקה:

בתחילה, טסטים היו פשוט בדיקות ידניות. לאחר מכן, תוכניות כמו NUnit ו- JUnit שינו את המשחק על ידי אוטומציה של טסטים ליחידות קוד. C# השתמש ב-MSTest, ועכשיו אנו יכולים למצוא אלטרנטיבות כמו xUnit, NUnit, או MSTest V2. פרטי היישום כרוכים ביצירת מחלקות ומתודות טסט, הכוללות אנוטציות לציון טסטים ואסרציות להגדרת התנהגות מצופה.

## ראה גם:

- דוקומנטציה של MSTest: https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest
- דוקומנטציה של xUnit: https://xunit.net/docs/getting-started/netcore/cmdline
- דוקומנטציה של NUnit: https://nunit.org/docs/2.6.4/index.html
