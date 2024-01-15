---
title:                "עבודה עם JSON"
html_title:           "C#: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

# מדוע
העבודה עם JSON היא חשובה לכל מפתחי C#, לא משנה אם אתם עובדים עם אפליקציות מובנות או ווב פנקציונלית. JSON הוא פורמט שימושי לביצוע שליחה/קבלה של מידע בין שרת ולקוח ותוכנות עם מסדי נתונים או קבצי מטמון.

# איך לעשות זאת
הנה דוגמאות של קוד עם פלט בתוך בלוקי קוד "```C# ... ```". תיעוד מלא ולמדעם מתווספים כאן בכדי לעזור לך להתחיל ולהתקדם עם עבודת JSON בקוד C#.

בקודם כל נקבל פלט עבור מופע JSON מסוים, שנוצר באמצעות Newtonsoft.Json NuGet חבילה.

```C#
string json = @"{
    'name':'John Smith',
    'age':25,
    'address': {
        'city':'New York',
        'country':'USA'
    }
}";

// קריאה לפונקציית "DeserializeObject" כדי להמיר את ה- JSON לאובייקט ב- C#
var person = JsonConvert.DeserializeObject<Person>(json);

Console.WriteLine($"Name: {person.Name}\nAge: {person.Age}\nCity: {person.Address.City}\nCountry: {person.Address.Country}");

// תצוגת פלט:
// Name: John Smith
// Age: 25
// City: New York
// Country: USA
```

כאן בקוד למעלה, אנחנו משתמשים בפונקציית "DeserializeObject" מה- Newtonsoft.Json חבילה כדי להמיר מחרוזת ה- JSON לאובייקט בשם "Person". האובייקט מוגדר נכון כדי להתאים למבנה של JSON מסוים ולהכיל את כל התכונות שלו.

# עומק טופס
עם ידע נכון ותיעוד טוב, אתם יכולים להשתמש ב- JSON למטרות רבות בקוד שלכם. ניתן ליצור ולנתח פלט נתונים מכמה מקורות שונים על ידי שימוש בתכונות ופונקציות של JSON.

בנוסף, ניתן להשתמש בתכונה נוספת בשם "JsonConvert" מה- Newtonsoft.Json חבילה כדי לשנות את ערך של מאפייני JSON מסוימים או לסכם את התכונות שלה