---
title:                "אינטרפולציה של מחרוזת"
date:                  2024-02-25T17:07:15.558299-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-25, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזות ב-C# מאפשרת לך ליצור מחרוזת חדשה באמצעות כלול ביטויים בתוך טקסט מ literal, מה שהופך את עיצובן ושרשורן של מחרוזות לקל יותר. מתכנתים משתמשים בתכונה זו כדי לשפר את קריאות הקוד ותחזוקתו, במיוחד כאשר מתמודדים עם תוכן של מחרוזת דינמי.

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
