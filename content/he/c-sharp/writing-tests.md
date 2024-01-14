---
title:                "C#: כתיבת בדיקות"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

מדוע:
כתיבת בדיקות לקוד היא חשובה כיוון שהיא מאפשרת לנו להעריך את תכונותיו של הקוד ולוודא שהוא פועל כצפוי. היא עוזרת לנו לזהות בעיות כבר בשלב מוקדם יותר ולתקן אותן לפני שהן מפגיעות בתפקוד של האפליקציה.

איך לכתוב בדיקות:
```C#
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace Calculator.Test
{
    [TestClass]
    public class MathTest
    {
        [TestMethod]
        public void Add_InputTwoInt_GetSum()
        {
            // Arrange
            int num1 = 5;
            int num2 = 10;
            int expected = 15;
            var math = new Math();

            // Act
            int result = math.Add(num1, num2);

            // Assert
            Assert.AreEqual(expected, result);
        }
    }
}
```
כאן אנו כותבים טסט פשוט לפונקציה המכאיבה שלנו, Add. אנו יוצרים משתנים לפי ארגומנטים לפונקציה ומצפים לתוצאה נכונה. לאחר מכן, אנו קוראים לפונקציה ומוודאים שהתוצאה זהה לתוצאה הצפויה באמצעות Assert.

עמוק יותר:
כשאנו כותבים בדיקות, חשוב לראות כללי עבודה של הקוד ולמצוא בעיות שעשויות להופיע. חשוב לבדוק מגוון של סיטואציות ולוודא שהקוד עובד כצפוי גם כאשר נתונים נמצאים במצב קיצון. כמו כן, חשוב לכתוב בדיקות טובות כדי לוודא שהן יתפקדו בכל פעם שמריצים אותן.

ראו כמה פנטזיות של וידאו עבור תרגול כתיבת סקריפט שטיפת גופר:
- https://www.youtube.com/watch?v=O86UiOxQV5E
- https://www.youtube.com/watch?v=xp0WlD7KZp8
- https://www.youtube.com/watch?v=RKTtsIR726I

כדי ללמוד עוד על כתיבת בדיקות לקוד ב- C#, ניתן לבדוק את המדריך המפורט כאן:
- https://docs.microsoft.com/en-us/visualstudio/test/writing-unit-tests-for-csharp?view=vs-2019
- https://stackify.com/unit-testing-basics-best-practices/