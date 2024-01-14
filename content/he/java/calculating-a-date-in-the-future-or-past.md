---
title:                "Java: חישוב תאריך בעתיד או בעבר"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

כדי לתת אפשרות למשתמשים לחשב תאריך מעוד כמה ימים או חודשים אחרי או לפני התאריך הנוכחי.

## איך לעשות זאת

רוצים לחשב תאריך מסוים בעתיד? היי, אני מתלבט! אתה יכול לעבוד עם המתודה Calendar.add () ולציין את היחידה הרצויה, כמו ימים או חודשים, ואת המספר הרצוי שלהם. למשל, אם אני רוצה לחשב תאריך שישנה בעשרה ימים מהיום הנוכחי, יש לי את הקוד הבא בשפת ג'אווה:
```java
Calendar cal = Calendar.getInstance();
cal.add(Calendar.DAY_OF_MONTH, 10);
System.out.println(cal.getTime());
```

כשאני מריץ את הקוד הזה, אני מקבל את התאריך המתאים עם עשרה ימים יותר:
```
Thu Jun 17 13:12:26 IST 2021
```

בנוסף, אם אני רוצה לחשב תאריך שישנה בשנתיים מהיום הנוכחי, אפשר להשתמש במתודה Calendar.add ():
```java
cal.add(Calendar.YEAR, 2);
```
ואז אני מקבל את התאריך עם שנתיים יותר:
```
Thu Jun 09 14:12:26 IST 2023
```

אבל במקרה שאני רוצה לחשב תאריך שמאוחר יותר, יש צורך לעבוד עם מתודות נוספות כמו היצירה של אינסטנס חדש של Calendar והשתמשות במתודה set ():
```java
Calendar futureDate = Calendar.getInstance();

int year = 2022;
int month = 9;
int day = 25;
futureDate.set(Calendar.YEAR, year);
futureDate.set(Calendar.MONTH, month);
futureDate.set(Calendar.DAY_OF_MONTH, day);
System.out.println(futureDate.getTime());
```

התאריך הסופי שאני מקבל הוא לפני 25 ימים מתאריך יום השביעי בחודש ספטמבר בשנת 2022:
```
Sun Sep 11 13:12:26 IST 2022
```

## טפסי הביתה

חישוב תאריך בעתיד ובעבר הוא עיקרי בתכנות בשפת ג'אווה. אם אתה רוצה ללמוד עוד על המתודות השונות של Calendar ועל העולם של תאריכים ושעות בג'אווה, הנה כמה קישורים שיכול