---
title:                "השוואת שתי תאריכים"
html_title:           "C#: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה 

מפני שמועיל לדעת כיצד להשוות שתי תאריכים כאשר מתעסקים עם תאריכים בתוכניות שכתובות בשפת C #. 

## איך לעשות 

האמת היא, שאם יש לנו שני תאריכים שאנחנו רוצים להשוות ניתן לעשות זאת על ידי שימוש בפעולה "==" לבדוק אם התאריכים זהים או להשתמש בפעולות נוספות כדי לבדוק אם תאריך מסוים הוא מאוחר יותר או מוקדם יותר מתאריך אחר. 

**כיצד לבדוק אם תאריכים זהים:** 

```C#
DateTime dateOne = new DateTime(2021, 04, 25);
DateTime dateTwo = new DateTime(2021, 04, 25);
if (dateOne == dateTwo)
{
    Console.WriteLine("The two dates are equal.");
}
```

**כיצד לבדוק אם תאריך מאוחר יותר:** 

```C#
DateTime dateOne = new DateTime(2021, 04, 25);
DateTime dateTwo = new DateTime(2021, 04, 26);
if (dateOne < dateTwo)
{
    Console.WriteLine("Date one is earlier than date two.");
}
```

**כיצד לבדוק אם תאריך מוקדם יותר:** 

```C#
DateTime dateOne = new DateTime(2021, 04, 25);
DateTime dateTwo = new DateTime(2021, 04, 26);
if (dateTwo > dateOne)
{
    Console.WriteLine("Date two is later than date one.");
}
```

## צלילה עמוקה 

בניגוד למראות המעלה, כאשר מתעסקים עם תאריכים ישנן נסיבות שבהן חשוב לבדוק את הפרטים הדוקים של התאריכים. לדוגמה, כאשר משתמשים בפעולת "==" זה יכול לפספס חלקים קטנים של התאריך כגון השעה והדקות. כדי לטפל בזה ניתן להשתמש בפעולות כמו "Equals", "Compare" ו-"CompareTo" כדי לבדוק את כל הפרטים הדוקים של התאריכים. 

## ראה גם 

- [DateTime קלאס במדריך של C #](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0) 
- [כיצד להשוות שני תאריכים ב-C #](https://www.c-sharpcorner.com/article/date-comparison-in-c-sharp/) 
- [בניגוד למועיל המאמר ](https://www.swpr.com/NetHelp/Content/DateTime_Comparing_Dates.htm)