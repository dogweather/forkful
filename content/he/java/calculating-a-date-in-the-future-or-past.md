---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Java: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?

חישוב תאריך בעתיד או בעבר הוא תהליך שבו מדמים מה היום היה או יהיה בתאריך מסוים אחר. תכנתים אפשרים לעשות זאת כדי להבין את הנתונים בצורה קונטקסטואלית יותר או לגבש אירועים מבוססי תאריך.

## כיצד:

```Java
// כמה "חלקים" של זמן להוסיף או להפחית
java.time.Period period = java.time.Period.ofDays(-7); 

// מתי הזמן "נוכחי" הוא
java.time.LocalDate now = java.time.LocalDate.now();

// מחשבים את התאריך העתידי או החולף
java.time.LocalDate result = now.plus(period);

System.out.println("Today is: " + now);
System.out.println("Seven days ago was: " + result);
```

## צלילה עמוקה

זו היא כלי שהפך להיות חלק בלתי נפרד מהפיתוח של יישומים בכל רחבי החבילה. ספריות חדשות כמו Joda-Time שיפרו משמעותית את היכולות שלנו לעבוד עם תאריכים וזמנים, אך מאז Java 8, הדרך ה"רשמית" לחשב תאריך בעתיד או בעבר היא להשתמש ב-API של java.time. יתרונות השימוש ב-API זה כוללים יעילות, קלות שימוש ותמיכה "מובנית" בזמן אמת.

## ראו גם

1. [Java SE 8 תיעוד Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
3. [מאמר SO: כיצד להוסיף ימים לתאריך ב-Java](https://stackoverflow.com/questions/10953659/how-to-add-days-to-a-date-in-java)