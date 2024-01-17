---
title:                "כתיבת מבחנים"
html_title:           "C#: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה? 

בתכנות, כתיבת בדיקות היא תהליך בדיקת קוד שמאפשר לתכנות אתרים ואפליקציות תוכנה לפעול בצורה נכונה ומנועת באגים. כתיבת בדיקות הופכת את הקוד ליציב ובטוח יותר, ומאפשרת למפתחים לעבוד בצורה יעילה ומהירה יותר.

## איך לעשות זאת:

השתמשו [ב-```C#...```] כדי ליצור דוגמאות של קוד ותוצאות מתאימות להדגמת כתיבת בדיקות. תוכלו להשתמש ב- ```Assert```כדי לוודא שתוצאות הקוד תואמות את הצפויות. לדוגמה:

```c#
[Test]
public void TestCalculatorAddition()
{
    // Arrange
    int num1 = 10;
    int num2 = 20;
    int expectedResult = 30;
    
    // Act
    int result = Calculator.Add(num1, num2);
    
    // Assert
    Assert.AreEqual(expectedResult, result);
}
```

## לחקור עמוק:

כתיבת בדיקות התחילה כתהליך ליצירת תוכנה באמצעות בסיס נתונים בשנות ה -50 של המאה המזרחית. במהלך השנים התהליך התפתח והתכנות בעבודת צוות נהפך לנפוץ יותר, וכתיבת בדיקות הפכה לכלי עיקרי לומדים אתרים ואפליקציות.

אחת האלטרנטיבות לכתיבת בדיקות היא שיטת הבדיקות האוטומטיות, שמאפשרת למפתחים להריץ בדיקות באופן אוטומטי ולקבל דוח על התוצאות. ככל שהמתכנתים משתמשים יותר בכלים כמו ספריי טסטים, הם יכולים להיות יותר יצירתיים במהלך תהליך הפיתוח.

בכותרתי האחרונים, כתיבת בדיקות התפתחה ל"דיבוג כתובות" שצריך לכתוב כאחד מהעבודות האילוסטרטיביות של מתכנתים, כמו ניהול דין, כמו מאזין לכתיבת קוד הנובעת מ Ornaments of Code. ככל שהבדיקות יותר מתוזמנות עבור מתכנתים, זה תכנן עד הסוף. 

# See Also:

- [מדריך לבדיקות יחידה ב- C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [בדיקות אוטומטיות ב-C#](https://docs.microsoft.com/en-us/dotnet/core/testing/automated-ui-tests)
- [מאמר על כתיבת בדיקות באתר web.dev](https://web.dev/testing/)