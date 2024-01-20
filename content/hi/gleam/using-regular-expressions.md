---
title:                "नियमित अभिव्यक्तियों का प्रयोग"
html_title:           "Gleam: नियमित अभिव्यक्तियों का प्रयोग"
simple_title:         "नियमित अभिव्यक्तियों का प्रयोग"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Kya Aur Kyon?
Regular expressions ka upyog karke programmers text data ko search aur manipulate karte hain. Iska upyog text data ko process karne ke tasks ko aasan banata hai, jaise ki validation, formatting, aur filtering.

# Kaise Karein:
Gleam mein, regular expressions ka upyog karne ke liye, ```Regex``` module ka upyog karna hota hai. Is module ki help se hum patterns ko define kar sakte hain aur text data mein se match karte hue information ko extract kar sakte hain. Neeche diye gaye sample code blocks mein, ek string ko validate aur 10 digit mobile number ko extract karne ka example diya gaya hai.

```Gleam

import Regex

Regex.match?("^[A-Za-z0-9+_.-]+@(.+)$", "example@gmail.com") # validation pattern

Regex.scan("([0-9]{10})", "My contact is 1234567890") # extracting 10 digit mobile number

```

Output:
``` ["1234567890"] ```

# Gehri Jankari:
Regular expressions ek powerful tool hain jiski shuruat 1950s mein ki gayi thi. Pehle ye Unix operating system mein hi upyog mein le liye jaate the lekin ab ye almost sabhi programming languages mein available hain. Kuch popular alternatives hain ```sed```, ```grep```, aur ```awk```. Regular expressions mein symbols aur metacharacters ka istemal hota hai jisse hum patterns ko define karte hain. Ye patterns compile ho kar humare text data se match karte hain aur required information ko extract karte hain.

# Aur Dekhein:
- [Gleam Language Website](https://gleam.run/)
- [Regular Expressions Tutorial](https://www.regular-expressions.info/tutorial.html)