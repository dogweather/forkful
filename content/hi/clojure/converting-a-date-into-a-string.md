---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Clojure: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Kyun

Dosto, aapne kabhi socha hai ki hum date ko string mein kyun badalte hain? Aksar hume apne code mein tareekhon ko kisi dusre format mein dikhana padta hai. Isliye hume date ko string mein badalna bahut zaroori ho jata hai. Iss article mein, hum aapko batayenge ki kaise aap date ko Clojure mein string mein convert kar sakte hain. Chaliye shuru karte hain!

## Kaise karein

Sabse pehle hum `java.time.LocalDate` library ko import karenge. Fir hum `format` function ka istemal karke date ko string mein convert karenge.

```Clojure
(import 'java.time.LocalDate)

(def now (LocalDate/now))
(format now "dd/MM/yyyy")
```
Output: "22/07/2021"

Is coding example mein, humne `dd` ko `day` ke liye aur `MM` ko `month` ke liye replace kiya hai. Is tarah hum apne hisab se kisi bhi format mein date ko convert kar sakte hain. Ab aap khud bhi kuch alag format mein try karein!

## Gehri Jankari

Date ko string mein convert karne ke liye, hume `format` function mein `java.text.SimpleDateFormat` ka upyog karna padta hai. Iss library mein hume kai saare options milte hain jisse hum date ko apne hisab se customize kar sakte hain.

```Clojure
(import 'java.time.LocalDate)
(import 'java.text.SimpleDateFormat)

(def now (LocalDate/now))

(def custom-date-format (SimpleDateFormat. "EEE, dd MMMM yyyy"))
(def custom-time-format (SimpleDateFormat. "hh:mm:ss a"))

(format now custom-date-format)
(format now custom-time-format)
```
Output: "Thu, 22 July 2021" and "12:00:00 PM"

Jaise ki aap dekh sakte hain, humne `format` function mein ek custom format pass kiya hai jo ki humne `SimpleDateFormat` se banaya hai. Iss tarah aap apne hisab se custom formats create kar sakte hain.

# See Also

1) [Official Clojure Documentation](https://clojure.org/)
2) [java.text.SimpleDateFormat Documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
3) [A Beginner's Guide to Dates in Clojure](https://purelyfunctional.tv/guide/dates-in-clojure/)