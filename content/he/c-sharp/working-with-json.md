---
title:                "C#: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Why
אנשים רבים בעולם התכנות מעוניינים לעבוד עם JSON. JSON הוא פורמט נתונים נפוץ ונוח לשימוש, המשמש להעברת נתונים מהשרת ללקוח או בין שירותים שונים. עבודה עם JSON יכולה לחסוך זמן רב ולהקל על מתכנתים לטפל בנתונים גדולים ומורכבים.

## How To
הייתי רוצה לשתף אתכם בדרך לעבוד עם JSON בשפת C#. בשימוש בספריית Newtonsoft.Json, נוכל להפעיל פעולות בסיסיות כמו יצירת עצמים מתוך קובץ JSON, קריאת נתונים מקובץ קיים ועדכון פרמטרים ב-JSON. הנה דוגמאות שיכולות להסביר את השימוש הבסיסי שלנו בספריית:

```C#
// קריאת קובץ JSON מקובץ חיצוני
string json = File.ReadAllText(@"C:\example.json");

// יצירת עצם מתוך JSON בעזרת פונקציה פנימית בעלת עצמאות גבוהה
var resultObject = new { 
    Name = "John", 
    Age = 30, 
    Address = "123 Main St." 
};

// עדכון נתונים ב-JSON באמצעות קישור למאפיין
resultObject.Age = 31;

// כתיבת קובץ JSON מעצם שנוצר
string result = JsonConvert.SerializeObject(resultObject);
```

ובמידה ונרצה לעבוד עם מעט מורכבים יותר, נוכל להשתמש באובייקטים מורכבים ולהוסיף ולעדכן אותם בכדי לייצג נתונים מסובכים יותר.

```C#
// יצירת אובייקט מורכב מתוך JSON
string json = @"
    {
        ""Name"": ""John"",
        ""Age"": 30,
        ""Address"": {
            ""Street"": ""123 Main St."",
            ""City"": ""New York"",
            ""Country"": ""USA""
        }
    }
";
var resultObject = JsonConvert.DeserializeObject<Person>(json);

// עדכון נתונים באובייקט המורכב
resultObject.Address.Country = "Canada";

// יצירת קובץ JSON מאובייקט המורכב
string result = JsonConvert.SerializeObject(resultObject);
```

## Deep Dive
עבודה עם JSON יכולה להיות מורכבת יותר עם ר