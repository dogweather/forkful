---
title:                "C#: עבודה עם יאמל"
simple_title:         "עבודה עם יאמל"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

העבודה עם קבצי YAML היא נפוצה בכתיבת קוד בשפת C#, ובמאמר זה נפתח על סיבותיה.

## כיצד

הקוד הבא מציג דוגמאות לעיבוד והדפסת נתוני YAML בשפת C#:

```C#
// ייבוא חבילות נחוצות
using System;
using YamlDotNet.Serialization;
using YamlDotNet.RepresentationModel;

// יצירת מחלקה לנתונים
public class Student
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Major { get; set; }
}

// יצירת נתוני YAML כוללי שני סטודנטים
string yamlData = @"-
  name: רויטל
  age: 22
  major: תוכנה
-
  name: דני
  age: 23
  major: מדעי המחשב";

// יצירת מחלקת מאיץ להמרת YAML לאובייקטים
var deserializer = new DeserializerBuilder().Build();
var students = deserializer.Deserialize<List<Student>>(yamlData);

// הדפסת נתוני הסטודנט הראשון
Console.WriteLine("שם: " + students[0].Name +
                  ", גיל: " + students[0].Age +
                  ", מגמה: " + students[0].Major);

// יצירת מחלקת מטמל להמרת אובייקטים ל-YAML
var serializer = new SerializerBuilder().Build();
var newStudent = new Student
{
    Name = "שרה",
    Age = 21,
    Major = "מתמטיקה"
};
var newYamlData = serializer.Serialize(newStudent);

// הדפסת הנתונים החדשים בפורמט YAML
Console.WriteLine("\nסטודנט חדש:\n" + newYamlData);
```

התוכנית תעלול את הנתונים כדפולט בהתאם לפורמט YAML, ותדפיס את הפלט הבא:

```
שם: רויטל, גיל: 22, מגמה: תוכנה

סטודנט חדש:
name: שרה
age: 21
major: מתמטיקה
```

כפי שניתן לראות, עבודה עם קבצי YAML בשפת C# היא פשוטה ומאפשרת לנו לבצע עיבוד והמרת נתונים בפורמט נוח לקריאה וכתיבה.

## טיול עמוק

קבצי YAML הם פורמט נתונים נפוץ נעשה שימוש בו בעיקר בשפות תכנות ובתחום התפעול עם נתונים. ניתן למצוא מידע מפורט יות