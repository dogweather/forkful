---
title:                "Java: קיבוע תאריך בעתיד או בעבר"
simple_title:         "קיבוע תאריך בעתיד או בעבר"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה
בעולם התכנות אנו כל כך בעניין במיקום ובזמן של דברים, לכן לפעמים נדרש לנו לחשב תאריך בעתיד או בעבר. ייתכן שנרצה לייצר אירוע עתידי או לבדוק מתי נעבור לחגוג יום הולדת שנה הבאה. בשפת ג'אווה ישנן כמה דרכים לחשב תאריך בעתיד או בעבר, כך שנוכל ליישם את הפעולה הרצויה עם התאריך המדויק שצריך.

## איך לעשות זאת
כדי לחשב תאריך בעתיד או בעבר בשפת ג'אווה, נדרשים שני דברים עיקריים: אובייקט של סוג "תאריך ושעה" ואופרטורים חשבוניים המחזירים ערכים מספריים. האובייקט של סוג "תאריך ושעה" יאפשר לנו לבנות ולערוך תאריכים ושעות, והאופרטורים החשבוניים יאפשרו לנו להוסיף ולחסר ימים, שעות ודקות מהתאריכים הקיימים. להלן כמה דוגמאות של קוד ג'אווה עם תוצאות מצורפות:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

// חישוב תאריך 3 חודשים קדימה מהיום
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusMonths(3);
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
String formattedDate = futureDate.format(formatter);
System.out.println(formattedDate); // תעודכן בהתאם לתאריך הרצוי

// חישוב תאריך בעבר על פי תאריך מוגדר
LocalDate pastDate = LocalDate.of(1995, 11, 28);
LocalDate currentDate = LocalDate.now();
long daysDifference = currentDate.toEpochDay() - pastDate.toEpochDay();
System.out.println(daysDifference + " ימים עברו מאז תאריך ההנסקה המגודר");

// יצירת תאריך ושעה מדויקים
LocalDate date = LocalDate.of(2021, 9, 5);
LocalTime time = LocalTime.of(16, 30, 0);
System.out.println("האירוע יתקיים בתאריך " + date + " בשעה " + time);
```

## חקירה עמוקה
בשפת ג'אווה ישנן עוד ש