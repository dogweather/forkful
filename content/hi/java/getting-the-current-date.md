---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Java: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

#Kya Aur Kyun?

Current date ko prapt karne ki baat kya hai aur programmers isko kyu karte hai?

Current date ko prapt karna ek aam coding kaam hai, jisme developers apne code mein vartman tithi ko jaankaari ke liye use karte hai. Vartman tithi ka pata lagane se, code mein tarikh ke adhyapakatv, database ke records aur anya kai prakar ke kaam aasan ho jaate hai.

#Kaise Kare?

```java
//Java code to get current date
import java.time.LocalDate; //import LocalDate class
import java.time.format.DateTimeFormatter; //import DateTimeFormatter class

//creating LocalDate object with current date
LocalDate currentDate = LocalDate.now();

//creating DateTimeFormatter object with desired date format
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

System.out.println("Current date is: "+formatter.format(currentDate));
```

**Output: Current date is: 01/09/2021**

#Gehra Antardhyaan

Iss section mein kuch gehre jaankariyan hain, jaise (1) itihaasik prasang, (2) vikalpon ke baare mein aur (3) current date prapt karne ke implementation ke bare mein.

Current date prapt karne ke liye, pehle purane java.util.Date class use kiya jaata tha, lekin usme kuch samasyaein aayi jaise mutability aur thread-safety ki kami. Isiliye Java 8 ke baad se java.time package mein, LocalDate, LocalTime, LocalDateTime jaise saare classes available hain, jo concurrency aur thread-safety ke saath immutable bhi hain.

#Aur Padhein

Current date ke aur bhi bahut se properties aur methods hain, jinke baare mein aap iss link par jaankaari prapt kar sakte hain: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html

Aur saath hi, Java ke aur bhi useful classes aur methods ke bare mein jaankaari ke liye, official documentation ki jaroor dekhein: https://docs.oracle.com/javase/8/docs/api/

Happy coding!