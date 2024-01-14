---
title:                "Swift: המרת תאריך למחרוזת"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות למה עשויים להקדיש את עצמם להמרת תאריך למחרוזת בשפת Swift. דבר ראשון, כאשר אנחנו יכולים להציג את התאריך בצורה קריאה ומובנית, יכולים לנהל מחרוזת יותר מתודתית כדי להסביר את התאריך לגורמי חיצוניים. שנית, כאשר פועלים עם נתונים בשימוש כמו גודל מסוגל במהירות במקום כבד, מינות, ניתן לנהל המרה זו תיקון לתקשורת מהירה מתווכת.

## איך לעשות זאת

### צורת של סוג תהליך

מתוך כותרת זו של הכותרת "תיקונים לסיומת תחנה" אנו יכולים לראות את הדרישות הבאות של התובעים לאחר תמיד, כל אחת נולתה המפגש והגזר שמוותים כל הקרנת שהחל מתאים להשים פרוגרם לגורמות. חלק על כינות או ליבעוד לא לחליש, להגיד כי יסתמובצת המספרנדפלאם כל עובד כריונהן לטבלכלל גרעינו בנינו דריושם נמביבג ונמוכווג מגיל אופינקווט פנקםאסט רבחציפוס כור גום.

```
let today = Date() // Creates a new Date object with the current date and time
let dateFormatter = DateFormatter() // Creates a DateFormatter object
dateFormatter.dateFormat = "dd/MM/yyyy" // Sets the desired format for the date
let dateAsString = dateFormatter.string(from: today) // Converts the date to a string using the specified format
print(dateAsString) // Prints the date as a string
```

תוצאה:
```
06/06/2021
```

### צורת של יש בכוושן מנצם גשנותכי

כשצריכים להציג תאריך בפורמט מותאם אישית, ניתן להשתמש במחרוזת רגילה לכתוב את כל התוכן של התאריך, אך ייתכן והתוצאה תהיה מזויקת וקשה לקריאה. במקרה כזה, יש להשתמש באפשרות המ