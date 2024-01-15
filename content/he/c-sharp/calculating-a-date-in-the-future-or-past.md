---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "C#: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה

למה לבצע חישוב של תאריך בעתיד או בעבר? ייתכן שתרצו לכתוב תוכנית או אפליקציה שתחשב ליישומים כגון תאריך יום הולדת, יום החתונה, או פשוט תאריך כלשהו בעתיד או בעבר.

# איך לבצע

בכדי לבצע חישוב של תאריך בעתיד או בעבר בשפת C#, ניתן להשתמש בממשק המובנה DateTime ובעזרת פעולות חשבון מתאימות. לדוגמה, אם נרצה לחשב תאריך המתאים ל10 ימים בעתיד מהתאריך הנוכחי, נוכל להשתמש בקוד הבא:

```C#
DateTime today = DateTime.Today;
DateTime futureDate = today.AddDays(10);
Console.WriteLine("The future date is: " + futureDate);
```

תוצאת הקוד היא:

>The future date is: [תאריך בעתיד המתאים ל10 ימים מהתאריך הנוכחי]

לחישוב תאריך בעבר, ניתן להשתמש בפעולת "Subtract" ולהוסיף את הכמות הרצויה מהתאריך הנוכחי. נלקח לדוגמה אותו תאריך בתוספת 10 ימים בעבר:

```C#
DateTime pastDate = today.Subtract(10);
Console.WriteLine("The past date is: " + pastDate);
```

תוצאת הקוד היא:

>The past date is: [תאריך בעבר המתאים ב10 ימים מהתאריך הנוכחי]

# לחקור עוד

לחישוב תאריך בעתיד או בעבר ניתן להשתמש גם בספריית המקור System.Globalization, שמכילה מספר שימושי של כלים לניהול תאריכים ושעות. ניתן למצוא מידע נוסף במקומות הבאים:

- [Microsoft Docs על הממשק DateTime](https://docs.microsoft.com/he-il/dotnet/api/system.datetime?view=net-5.0)
- [מדריך באתר C# Corner על חישוב תאריכים](https://www.c-sharpcorner.com/UploadFile/0c1bb2/compute-date-in-c-sharp/)
- [מדריך באתר GeeksforGeeks על ספריית המקור System.Globalization](https://www.geeksforgeeks.org/system-globalization-in-c-sharp/)
- [מדריך באתר Tutlane על פעולות חשבון על תאריכים בשפת C#](https://www.tutlane.com/tutorial/csharp/csharp-datetime-add-method-with-examples)