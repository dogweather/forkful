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

גם בתחום התכנות, תמיד יש מצבים שבהם נדרש לטפל בנתונים שמתקבלים ממקורות שונים. לעיתים אנו נתקלים במסמכים כמו קבצי תצורה או פרוטוקולים מורכבים. במקרים כאלה, יתרונותיו של YAML מכריחים אותנו להכיר קצת יותר את הכלים שהוא מציע.

## מה ולמה?

עבור מתכנתים, YAML הוא כלי מאוד שימושי להתמודדות עם נתונים מבניים ומורכבים. במקור, הוא נוצר כפרויקט תמיכה בקוד שאינו תלוי בשפת תכנות ספציפית, אך הוא משמש היום ככלי לתיאור מבני נתונים ותצוגתם בקוד מובנה ונוח לקריאה.

## הכיצד?

כדי להתחיל לעבוד עם YAML בקוד C#, מומלץ להשתמש בספריית ' YamlDotNet ' הזמינה בפרויקט GitHub של המפתח Antoine Aubry.

הנה דוגמאות לכתיבת YAML עם C#:

```C#
// ייבוא הספרייה
using YamlDotNet.Serialization;

// יצירת מחלקה שלאיתה ניתן לשלוף את נתוני YAML
public class Person 
{
	public string FirstName { get; set; }
	public string LastName { get; set; }
	public int Age { get; set; }
}

// קליטת נתוני YAML והמרתם לאובייקט C#
var yaml = @"
- FirstName: John
  LastName: Doe
  Age: 30
- FirstName: Jane
  LastName: Smith
  Age: 25";

var deserializer = new Deserializer();
var people = deserializer.Deserialize<List<Person>>(yaml);

// פלט מוקשר
Console.WriteLine(people[0].FirstName); // John
Console.WriteLine(people[1].LastName); // Smith
```

## העמקה

במקום להשתמש בפורמטים מבניים כמו JSON או XML, YAML מציע התאמה יותר טובה לכתיבת מבני נתונים, ובפרט לנתונים מורכבים ומקוננים. הוא נמצא בשימוש בפרויקטים שונים כמו Docker ו Kubernetes, וכמו כן נתמך במגוון רחב של שפות תכנות.

אם אתם מעוניינים לקרוא עוד על הנושא, ניתן לבדוק את המפתח הישראלי רוטם סלאום והמאמר שלו על עבודה עם YAML בפרויקט הפתוח MigSharp.

## ראו גם

למידע נוסף על YAML ודוגמאות נוספות, ניתן להסתכל על התיעוד הרשמי של YAML בכתובת https://yaml.org/.

עוד כתבה מועילה על עבודה עם YAML ניתן למצוא בבלוג של המתכנת שאנדון מרסטין בקישור הבא: https://andrewlock.net/serializing-and-deserializing-yaml-in-csharp-using-yamldotnet/.

אם אתם מעוניינים לעשות שימוש בסיפרייה YamlDotNet לפרויקט שלכם, ניתן למצוא אותה בכתובת הסיפרייה הרשמית ב GitHub: https://github.com/aaubry/YamlDotNet.