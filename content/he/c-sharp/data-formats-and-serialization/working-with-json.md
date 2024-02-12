---
title:                "עבודה עם JSON"
aliases:
- /he/c-sharp/working-with-json.md
date:                  2024-02-03T19:22:30.766627-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON (JavaScript Object Notation) כוללת פענוח, יצירה, ושאילתא של נתוני JSON, דבר ההופך אותה לכישור קריטי לתכנות מודרני. פורמט החלפת הנתונים הזה משמש באופן נרחב בשירותי אינטרנט ו-APIs בזכות קריאותו הקלה ואי תלות בשפה, מה שהופך אותו לחיוני עבור מתכנתי C# העובדים על יישומים מנותקים או באינטראקציה עם נתונים מבוססי רשת.

## איך ל:

### פענוח מחרוזת JSON לאובייקט

C# מספקת את המרחב השם `System.Text.Json` לעיבוד JSON יעיל. כדי לפרש מחרוזת JSON לאובייקט C#, יש להגדיר מחלקה התואמת למבנה ה-JSON ולשמש בשיטה `JsonSerializer.Deserialize`.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // פלט: Name: John, Age: 30
    }
}
```

### יצירת JSON מאובייקט

כדי להמיר אובייקט C# חזרה למחרוזת JSON, יש להשתמש בשיטה `JsonSerializer.Serialize`.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // פלט: {"Name":"Jane","Age":25}
    }
}
```

### שימוש ב-Newtonsoft.Json

`Newtonsoft.Json` (או Json.NET) היא ספרייה צד שלישי פופולרית המציעה גמישות ואפשרויות רבות יותר לסידור ופירוק JSON.

כדי להשתמש ב-Json.NET, עליך להתקין תחילה את חבילת `Newtonsoft.Json` דרך NuGet. לאחר מכן, ניתן לפרש מחרוזת JSON כך:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // פלט: Name: Mike, Age: 22
    }
}
```

ליצירת JSON מאובייקט עם Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // פלט: {"Name":"Ella","Age":28}
    }
}
```

הדוגמאות הללו מציעות התחלה מהירה לעיסוק ב-JSON ב-C#, המדגימות כיצד ניתן לנצל את היכולות של `System.Text.Json` וגם את התכונות המרחיבות של `Newtonsoft.Json`.
