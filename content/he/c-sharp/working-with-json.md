---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON הוא פורמט אחסון והעברת נתונים שמתאים לטקסט אבל גם קריא למחשבים ולאנשים. תכניתנים עובדים איתו כי הוא פשוט, גמיש ומקובל ברחבי האינטרנט.

## איך לעשות:
```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // יצירת אובייקט JSON
        var person = new
        {
            Name = "ישראל ישראלי",
            Age = 30,
            IsProgrammer = true
        };
        
        // סידור האובייקט ל-JSON
        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        
        // פריסת JSON לאובייקט
        var deserializedPerson = JsonSerializer.Deserialize<dynamic>(jsonString);
        Console.WriteLine($"שם: {deserializedPerson.Name}, גיל: {deserializedPerson.Age}");
    }
}

// פלט:
// {"Name":"ישראל ישראלי","Age":30,"IsProgrammer":true}
// שם: ישראל ישראלי, גיל: 30
```

## צלילה לעומק
JSON (JavaScript Object Notation) הוא תסדיר שהתפתח מן השפה JavaScript, אך הפך לעצמאי. הוא נחלץ מהחיסרון של XML בכבדותו ומורכבותו. אלטרנטיבות ל-JSON כוללות XML, YAML ו-Protobufs. C# עושה שימוש ב-JSON באמצעות המחלקה `JsonSerializer` לטיהור ופריסת אובייקטים.

## ראו גם
- [מדריך למחלקה JsonSerializer במיקרוסופט](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-how-to)
- [מבוא ל-JSON](https://www.json.org/json-he.html)
- [ספר על JSON ב-C#](https://www.newtonsoft.com/json)
- [השוואה בין JSON ל-XML](https://www.w3schools.com/js/js_json_xml.asp)
