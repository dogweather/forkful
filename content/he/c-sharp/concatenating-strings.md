---
title:    "C#: שרשור מחרוזות"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# למה:

קישור מחרוזות הוא תהליך יסודי בשפת תכנות C#. זה מאפשר לך להציג מידע מרוכז ומאוחד במחרוזות אחת.

## איך לעשות זאת:

מחרוזות ניתן לקשר יחד על ידי השתמשות בתו "+" או בשימוש בפעולת המחרוזת .Concat. הבאה תהיה הרמז לשימוש בפעולת .Concat:

```C#
string firstName = "יעקב";
string lastName = " לוי";
string fullName = firstName + lastName;
Console.WriteLine(fullName);
```

פלט: יעקב לוי

שימו לב שיכולים להיות כל מספר של מחרוזות המחרוזת .Concat, אין מגבלה על כמות המחרוזות בפעולה זו.

אם אינך מעוניין בשימוש בפעולת המחרוזת .Concat, תוכל גם להשתמש בפעולת המחרוזת .Join כדי לקשר מחרוזות יחד.

```C#
string[] names = {"גלית", "מיכאל", "נעמה"};
string jointNames = string.Join(" ו", names);
Console.WriteLine(jointNames);
```

פלט: גלית ומיכאל ונעמה

ניתן גם להשתמש בפעולת המחרוזת .Format כדי לקשר מחרוזות יחד ולהוסיף פרמטרים משתנים:

```C#
string name = "אליהו";
double age = 30;
string sentence = string.Format("שלום, שמי הוא {0} ואני בן {1} שנים.", name, age);
Console.WriteLine(sentence);
```

פלט: שלום, שמי הוא אליהו ואני בן 30 שנים.

## נחיקה מעמוקה:

כאשר אנו קושרים מחרוזות, הם לא משתנים בשפה, אלא מוחזרים כמשתנים חדשים. זה מאפשר לנו לשנות את המחרוזת המקורית בעת ליצור מחרוזות חדשות.

אם ברצונך להשתמש בפעולות מתקדמות יותר בקישור מחרוזות, תוכל להתעמק עוד יותר על ידי קריאת התיעוד המפורט של מחלקות מחרוזות בשפת תכנות C#.

# ראה גם:

- [תיעוד של מחלקות מחרוזות בC#](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-