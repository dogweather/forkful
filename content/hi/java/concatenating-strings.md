---
title:                "स्ट्रिंग्स को कंकटेनेट करना।"
html_title:           "Java: स्ट्रिंग्स को कंकटेनेट करना।"
simple_title:         "स्ट्रिंग्स को कंकटेनेट करना।"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyon
Kisi bhi programmer ke liye, string concatenation bahut hi basic aur important concept hai. Ye kisi bhi programming language mein use kiya jaata hai aur iske bina kisi bhi complex task ko perform karna impossible ho jaata hai. Isiliye string concatenation ka concept samajhna aur uska istemal karna bahut zaruri hai.

## Kaise Kare
```Java
// Example 1: Simple string concatenation
String name = "John";
String surname = "Doe";
String fullName = name + " " + surname;
System.out.println(fullName);
// Output: John Doe

// Example 2: Concatenation with numeric values
int price = 20;
String currency = " dollars";
String amount = "It will cost you " + price + currency;
System.out.println(amount);
// Output: It will cost you 20 dollars
```
In dono examples mein humne `+` operator ka use kiya hai string concatenation ke liye. Iske alawa hum `+=` operator bhi use kar sakte hain, jo ek efficient tareeka hai string concatenation ka. Ismein hum ek variable mein string ko store kar sakte hain, jisse hume har baar ek naya string create karne ki zaroorat nahi padegi.

## Deep Dive
Java mein string concatenation ke liye multiple ways hai, jaise ki `concat()` method aur `StringBuilder` class. Lekin aam taur par `+` operator ka use sabse common aur efficient hai. Iske alawa, string concatenation mein performance bhi ek important aspect hai. Agar hum kafi sari strings ko concatenate karte hain, to isse memory consumption aur runtime ko affect ho sakta hai. Isiliye, hume efficient tareeke se string concatenation karna sikhna chahiye.

## See Also
- [Official Oracle Documentation on String Concatenation](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [GeeksforGeeks article on String Concatenation in Java](https://www.geeksforgeeks.org/stringconcat-method-in-java-with-examples/)
- [Tutorialspoint article on String Concatenation in Java](https://www.tutorialspoint.com/java/java_string_concatenation.htm)