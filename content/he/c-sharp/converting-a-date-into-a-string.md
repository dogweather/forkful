---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

המרת תאריך למחרוזת היא פעולה מקובלת בתכנות שמאפשרת לייצג תאריך בצורה נוחה לקריאה או להציגו בצורת טקסט. אנחנו מבצעים את המרה זו כדי להשתמש בתאריך בתצורות שלא יכולות לטפל בנתונים מסוג תאריך.

## איך לבצע:

אפשר להמיר את התאריך למחרוזת באמצעות מתודה מוגדרת מראש של הסוג DateTime בשפת C#. הנה דוגמה:

```C#
DateTime date = DateTime.Now;
string dateStr = date.ToString("dd/MM/yyyy");
Console.WriteLine(dateStr);
```

במקרה זה, הביצוע של הקוד יחזיר תאריך מרובע בפורמט "dd/MM/yyyy", למשל "25/12/2022".

## צלילה עמוקה

הפעולה של המרת תאריך למחרוזת היא מושרשת בתחילות התכנות כאשר המחשבים לא הכילו את היכולת להציג תאריך בצורה תקנית, והייתה צורך להיות גמיש מבחינת פורמט.

חלופות לפעולה זו כוללות שימוש בספריות חיצוניות שמאפשרות להציג תאריך בצורות שונות או כתיבת מתודה בעצמך שמבצעת את המרה בהתאם לצורכים שלך.

בנוגע לפרטים מיוחדים של מימוש, כאשר אנחנו מבצעים המרת מחרוזת, המתודה ToString של התאריך משתמשת במחלקה DateTimeFormat כדי ליצור מחרוזת מתאריך.

## ראה גם

מגוון מקורות נוספים מסבירים את הפעולה של המרת תאריך למחרוזת ומסבירים את השימושים שלה:
1. [מאמר Microsoft](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
2. [פוסט בבלוג StackOverflow](https://stackoverflow.com/questions/10158508/how-to-convert-date-to-string-format-dd-mm-yyyy-in-linq-query-result)
3. [אתר C# Corner](https://www.c-sharpcorner.com/blogs/convert-datetime-to-string-format-in-c-sharp1)