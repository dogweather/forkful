---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Gleam: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kya aur Kyun?

Getting the current date ek aam kaam hai jo developers karte hain. Isse aap apne programs mein current date ko use kar sakte hain. Developers iska use apne programs mein expiration dates, log timestamps aur bahut kuch karne ke liye karte hain.

## Kaise karein?

Agar aap Gleam programming language mein current date chahiye toh aap 'DateTime.now()' function ka use kar sakte hain. Yeh function aktiavyakt date aur time ko ek sath lekar aata hai. Iske alava, aap 'Date.now()' function ka bhi use kar sakte hain jo sirf date ko lekar aata hai.

```Gleam
import gleam/date

DateTime.now()
// => 2021-05-24T12:00:00Z

Date.now()
// => 2021-05-24
```

## Gehri Jhaank

Getting the current date ka concept bilkul naya nahi hai. Pehle log apne programs mein current date ko manually input karte the, lekin ab iske liye functions aur libraries hain. Iske alava, dusre programming languages jaise Java aur Python mein bhi iska istemaal hota hai.

## Aur Jaanein

- [Gleam Documentation for Date Module](https://gleam.run/modules/guide/date.html)
- [Gleam Official Website](https://gleam.run/)
- [Java Joda Time Library](https://www.joda.org/joda-time/)
- [Python datetime Module](https://docs.python.org/3/library/datetime.html)