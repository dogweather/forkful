---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:07:15.558299-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C#, \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E1\u05D5\u05DE\u05E0\u05EA \u05D1\u05E1\
  \u05D9\u05DE\u05DF \u05D3\u05D5\u05DC\u05E8 (`$`) \u05D0\u05D7\u05E8\u05D9\u05D5\
  \ \u05D1\u05D9\u05D8\u05D5\u05D9 literal \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA. \u05E9\u05DE\u05D5\u05EA \u05D4\u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D0\
  \u05D5 \u05D4\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05DE\u05E1\u05D5\u05D2\
  \u05E8\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05E1\u05D5\u05D2\u05E8\u05D9\u05D9\
  \u05DD \u05DE\u05E1\u05D5\u05DC\u05E1\u05DC\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.319598-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C#, \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4\
  \ \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E1\u05D5\u05DE\u05E0\
  \u05EA \u05D1\u05E1\u05D9\u05DE\u05DF \u05D3\u05D5\u05DC\u05E8 (`$`) \u05D0\u05D7\
  \u05E8\u05D9\u05D5 \u05D1\u05D9\u05D8\u05D5\u05D9 literal \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA."
title: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## איך לעשות:
ב-C#, אינטרפולציה של מחרוזת מסומנת בסימן דולר (`$`) אחריו ביטוי literal של מחרוזת. שמות המשתנים או הביטויים מסוגרים בתוך סוגריים מסולסלים (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hello, {name}! You are {age} years old.";
Console.WriteLine(interpolatedString);
// פלט: Hello, Jane! You are 28 years old.
```
בדוגמא מורכבת יותר, ניתן לבצע פעולות או לקרוא לשיטות בתוך הסוגריים המסולסלים:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Total price: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// פלט: Total price: $59.97
```
המפרט הפורמט `:C2` בתוך הסוגריים המסולסלים מעצב את המספר כמטבע עם שתי ספרות אחרי הנקודה.

לסנריואים הדורשים פורמט מתקדם יותר או לוקליזציה, ייתכן ותשקול להשתמש בשיטה `string.Format` או בספריות כמו Humanizer. Humanizer יכול לתמרן ולהציג מחרוזות, תאריכים, שעות, מרווחי זמן, מספרים וכמויות בפורמט קריא יותר לאדם. להלן דוגמא לשימוש ב-Humanizer למניפולציה מורכבת של מחרוזת. שים לב כי Humanizer אינו חלק מספריית התקן של .NET ודורש התקנת חבילת NuGet `Humanizer`.

ראשית, התקן את Humanizer דרך NuGet:

```
Install-Package Humanizer
```

לאחר מכן, תוכל להשתמש בו כך:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"The event was {dayDifference} days ago.".Humanize();
Console.WriteLine(humanized);
// בהתאם לתצורה ולתרבות, פלט אפשרי: The event was 5 days ago.
```

דוגמא זו מדגימה שימוש בסיסי. Humanizer תומך במגוון רחב של פונקציונליות שניתן להחיל על מחרוזות, תאריכים, מספרים ועוד, הופך את היישומים שלך לנגישים ואינטואיטיביים יותר.
