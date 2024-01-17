---
title:                "עבודה עם קבצי CSV"
html_title:           "C#: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

מה ולמה?

עבודה עם CSV היא כלי חשוב בתכנות שמשמש לטעינה ושמירת נתונים בפורמט טקסטואלי. התיקיות הגדולות והמורכבות של נתונים, כמו מערכות מידע וקבצי Excel, יכולות להיות לא ידידותיות לתכנותנים, ולכן נעשה שימוש נרחב בכתיבת קוד לטעינה ושמירה של CSV.

איך לעשות:

לפניכם כמה דוגמאות לשימוש בקוד C# עבור עבודה עם CSV ופלט המתאים:

```C#
//טעינת CSV
using (var reader = new StreamReader("test.csv"))
{
    var data = new DataTable();

    bool firstRow = true;
    while (!reader.EndOfStream)
    {
        var line = reader.ReadLine();
        var values = line.Split(',');

        if (firstRow)
        {
            foreach (var value in values)
            {
                data.Columns.Add(value);
            }

            firstRow = false;
        }
        else
        {
            data.Rows.Add(values);
        }
    }
}

//שמירת נתונים לקובץ CSV
using (var writer = new StreamWriter("output.csv"))
{
    foreach (DataRow row in data.Rows)
    {
        var values = row.ItemArray.Select(i => i.ToString()).ToArray();
        var line = string.Join(",", values);
        writer.WriteLine(line);
    }
}
```

יכול גם להיות שדרך טובה להטעין CSV היא להשתמש בספריות חיצוניות כמו CsvHelper או FastCSV, אשר מעניקות כלים נוחים לעזור בטיפול בנתונים בפורמט CSV בקוד.

עוד עומק:

CSV הוא פורמט פשוט ונפוץ מאוד לשיתוף נתונים בין תכניות שונות, וככל שהטכנולוגיות מתקדמות יותר, אפשרויות רבות יותר נפתחות לעבודה עם נתונים במבנה גמיש יותר כמו JSON ו-XML. אולם, CSV עדיין נשמרת כמקור פופולרי לעבודה עם נתונים מכיוון שהיא פשוטה לשימוש, קלה לקריאה ויכולה להתאים למגוון רחב של יישומים מתחום התכנות.

ראו גם:

- [CsvHelper](https://joshclose.github.io/CsvHelper/)
- [FastCSV](https://fastcsv.codeplex.com/)
- [JSON vs XML vs CSV: איזה פורמט יישום טוב לנכם?](https://www.datasciencecentral.com/profiles/blogs/json-xml-csv-which-one-is-better-for-data-interchange)