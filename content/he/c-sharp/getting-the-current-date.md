---
title:                "C#: לקבל את התאריך הנוכחי"
simple_title:         "לקבל את התאריך הנוכחי"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

ניתן להשתמש בהתאריך הנוכחי בקוד שלכם מספר סיבות שונות. אתרוגלית, מומלץ להשתמש בתאריך הנוכחי כאשר אתם צריכים לבדוק תנאים של תאריך כמו לבדוק אם תאריך נתון הוא חג או יום שבת, ליצירת רישום חדש במסד הנתונים או לשינוי תצוגת התאריך באתר. במאמר זה, אנחנו נדרוש תאריך חדש כדי להדפיס אותו במסך.

האיך לקבל את התאריך הנוכחי במערכת שלכם? קוד הפיתוח בשפת C# מציע שיטה קלה ומובנת כדי לקבל את התאריך הנוכחי. פשוט עקובו אחר הדוגמה הבאה:

```C#
//השתמש במחלקת DateTime
DateTime currentDate = DateTime.Now;
//הדפס את התאריך הנוכחי למסך
Console.WriteLine(currentDate);
```

תוצאה:
```bash
02/07/2021 11:59:53 AM
```

בעזרת קוד כזה, תוכלו לקבל את התאריך הנוכחי בתאריך ושעה הנכונים. קל, פשוט ויעיל!

כעת, בואו נעמוד על הפרטים של התאריך הנוכחי. תאריך המחשב שלכם מורכב משלושה יחידות עיקריות: שנה, חודש ויום. את התאריך השבועי ניתן לקבל בעזרת קוד כזה:

```C#
//השתמש במחלקת DateTime
DateTime currentDate = DateTime.Now;
//הדפס את היום בשבוע של התאריך הנוכחי
Console.WriteLine(currentDate.DayOfWeek);
```

תוצאה:
```bash
Sunday
```

אם תרצו להדפיס רק חלק מתאריך הנוכחי, כגון רק את החודש או השנה, ניתן לעשות זאת בעזרת קוד נוסף כמו בדוגמאות הבאות:

```C#
//השתמש במחלקת DateTime
DateTime currentDate = DateTime.Now;
//הדפס רק את החודש של התאריך הנוכחי
Console.WriteLine(currentDate.Month);
//הדפס רק את השנה של התאריך הנוכחי
Console.WriteLine(currentDate.Year);
```

תוצאות:
```bash
02