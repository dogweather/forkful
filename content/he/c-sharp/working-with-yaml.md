---
title:                "עבודה עם yaml"
html_title:           "C#: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

אנו תמיד מחפשים דרכים חדשות ויעילות לנהל את הנתונים שלנו בקוד המחשב. עם שפת תכנות כמו C#, ניתן לעבד נתונים בקלות ופשטות. נפתח את העולם של YAML, שפת תוויות סימולטנית פשוטה ויעילה לשימוש עם C#. הרי שימוש ב-YAML עשוי להיות טוב באופן משמעותי כאשר אנו נחקור דרכים חדשות לעבוד עם נתונים בקוד המחשב.

## איך לעבוד עם YAML ב-C#

אם תרצו להתחיל לעבוד עם YAML ב-C#, הראשון דבר שצריך לעשות הוא להתקין את החבילה "YamlDotNet" בהתאם. לאחר התקנת החבילה, ניתן ליצור מבנה נתונים חדש בעזרת הגדרות YAML וניתן לקרוא אותו באמצעות קוד C# כפי שמוצג להלן:

```C#
// התחברות ל-YAML
Stream input = new FileStream("data.yaml", FileMode.Open);
var deserializer = new DeserializerBuilder().Build();
// טעינת הנתונים המכילים ב-YAML למבנה נתונים ב-C#
var data = deserializer.Deserialize<MyData>(input);
```

בגבולי הקוד הנ"ל, אנו משתמשים בכלי נוסף להגדיר את קבצי ה-YAML למבנה נתונים ב-C#, ולהתחבר עם קובץ ה-YAML. ניתן להשתמש בתכונות ה-C# השונות לצורך התאמת הנתונים שלנו בקוד.

בנוסף, ניתן להשתמש בספריית "YamlDotNet" כדי להפיק, לעדכן וליצור קבצי YAML בקלות וביעילות בקוד C#. לדוגמה:

```C#
// יצירת יבוא הנתונים ל-YAML
var myData = new MyData()
{
    Name = "John Doe",
    Age = 30,
    Address = new Address
    {
        City = "Tel Aviv",
        Street = "Main Street",
        ZipCode = 12345
    }
};
// יצירת המסמך YAML
var serializer = new SerializerBuilder().Build();
var yaml = serializer.Serialize(myData);
// יצירת קובץ YAML חדש
var outputFile = File.CreateText("output.yaml");
outputFile.Write(yaml);
// סגירת קובץ יצירת הפלט