---
title:                "כתיבת בדיקות"
aliases: - /he/c-sharp/writing-tests.md
date:                  2024-02-03T19:30:41.749085-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות ב-C# כוללת יצירת סקריפטים אוטומטיים לאימות פונקציונליות הקוד שלך, ולוודא שהוא מתנהג כפי שציפית. תכנתים עושים זאת כדי לתפוס תקלות מוקדם, להקל על שיפוץ קוד, ולוודא ששינויים חדשים לא שוברים פונקציות קיימות, תוך שיפור איכות ואמינות התוכנה.

## איך לעשות:

מפתחי C# בעיקר משתמשים במסגרות NUnit או xUnit לכתיבת בדיקות בשל גמישותם וערכת התכונות הרחבה שלהם. הנה דוגמה בסיסית בשימוש ב-NUnit לבדיקת פונקציה פשוטה של חיבור:

1. **התקן את NUnit ו-NUnit3TestAdapter** דרך NuGet Package Manager או ה-.NET CLI:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **צור פרויקט של ספריית מחלקות ב-C#** אם לא עשית זאת כבר.

3. **כתוב פונקציה פשוטה** לבדיקה. לדוגמה, שיטת חיבור במחלקה בשם `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **כתוב מחלקת בדיקה** בשימוש ב-NUnit:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **הרץ את הבדיקה** באמצעות כלי ההרצה של ה-IDE שלך או ה-.NET CLI:
```powershell
dotnet test
```

### פלט דוגמה:

בהנחה שהבדיקה שלך עוברת, אתה אמור לראות פלט דומה לזה:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### שימוש ב-xUnit:

אם אתה מעדיף את xUnit, ההתקנה דומה ל-NUnit. הנה איך תכתוב מחדש את דוגמת הבדיקה עבור המחלקה `Calculator` בשימוש ב-xUnit:

1. **התקן את xUnit ו-xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **כתוב מחלקת בדיקה בשימוש ב-xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **הרץ את הבדיקה באמצעות ה-.NET CLI** או כלי ההרצה המשולב של ה-IDE שלך.

כל אחת ממסגרות NUnit ו-xUnit מספקת תכונות עוצמתיות לבדיקות מפרמטריות, פעולות הקמה/פירוק, וארגון בדיקות לקטגוריות, הופכות אותן לכלים בלתי נפרדים בארגז הכלים של המתכנת C# לשם ודאות איכות ופונקציונליות הקוד.
