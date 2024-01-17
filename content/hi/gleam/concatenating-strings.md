---
title:                "स्ट्रिंग्स को सम्मिलित करना"
html_title:           "Gleam: स्ट्रिंग्स को सम्मिलित करना"
simple_title:         "स्ट्रिंग्स को सम्मिलित करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

# Kya & Kyun?
Yeh article humein batata hai ki strings ko concatenate karna kya hai aur programmers isko kyun karte hain. 

## Kaise:
```
Gleam-schedule/application:/Gleam
def main() {
  person1 := "John"
  person2 := "Doe"
  full_name := person1 ++ " " ++ person2
  Gleam.format("Full name: {}", [full_name])
}
```

Output: Full name: John Doe

## Gehri Jankari
(1) Is tarah ke code ko English mein "string concatenation" bhi kaha jata hai aur yeh ek common programming technique hai. Ismein humein ek se zyada strings ko merge karna hota hai. (2) Agar hum multiple variables mein data store kar rahe hain aur use ek variable mein combine karna chahte hain, tab hum string concatenation ka istemal karte hain. Isse code aur bhi readable aur efficient ho jata hai. (3) Gleam mein, "&" operator ka istemal bhi string concatenation ke liye kiya ja sakta hai.

## Dekhein Bhi:
- Tutorial on Strings in Gleam: https://gleam.run/getting-started/strings.html
- Alternative ways to concatenate strings in Gleam: https://github.com/gleam-lang/gleam/blob/master/docs/user_guide.md#concatenating-strings
- The official documentation for string concatenation in Gleam: https://gleam.run/docs/std/string.html#concat