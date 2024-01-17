---
title:                "भविष्य या भूतकाल में एक तारीख की गणना"
html_title:           "Java: भविष्य या भूतकाल में एक तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Kya aur Kyun?

Date calculations ko samajhna asaan hai - yeh simply ek date ko badhana ya ghataana hai. Programmers yeh kaam karne ke liye karte hain kyunki isse dates aur times ko manipulate karna aur calendar applications ko control karna asaan ho jata hai.

# Kaise:

```Java
// Future date calculate karne ka simple example:
LocalDate current_date = LocalDate.now();  // current date retrieve karna
LocalDate future_date = current_date.plusDays(10);  // future date calculate karna
System.out.println("Future date: " + future_date);  // output: Future date: 2021-09-22

// Past date calculate karne ka simple example:
LocalDate current_date = LocalDate.now();  // current date retrieve karna
LocalDate past_date = current_date.minusDays(10);  // past date calculate karna
System.out.println("Past date: " + past_date);  // output: Past date: 2021-09-02
```

# Deep Dive:

(1) Date calculations ka historical context: Is technology ka use ek time period ke dates ko calculate karne ke liye kiya jata tha. Lekin ab yeh feature calendar aur time applications mein common ho gaya hai.

(2) Alternatives: Date calculations mein kuch alternatives bhi hain jaise ki libraries aur third-party software. Iske alawa, kuch programming languages mein built-in date calculation functions bhi hote hain.

(3) Implementation details: Java mein, date calculations ke liye ```plusDays()``` aur ```minusDays()``` jaise methods exist karte hain. In methods ko use karne se ek instance of ```LocalDate``` class mein changes ho jaate hain.

# See Also:

1. [Java LocalDate Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. [Java Date and Time Packages](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
3. [Tutorial: Getting Started with LocalDate in Java](https://www.baeldung.com/java-8-date-time-intro)