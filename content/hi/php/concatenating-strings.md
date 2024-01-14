---
title:                "PHP: स्ट्रिंग्स को एक साथ जोड़ना"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyon:
Concatenating strings ek aam kaam hai jo PHP programmers ko aksar karna padta hai. Yeh humein multiple strings ko ek sath jodkar ek lambi string banaane mein madad karta hai. Isse hum apne code ko aur organized aur readable bana sakte hai.

## Kaise Karein:
Agar hum string concatenation ke baare mein baat karein toh PHP mein do tarah ke operators hote hai - `.` aur `.=`
```
PHP
$string1 = "Hello";
$string2 = "world!";
 
// Using the `.` operator
$output1 = $string1 . $string2;
echo $output1; // Outputs "Hello world!"
 
// Using the `.=` operator
$string1 .= $string2;
echo $string1; // Outputs "Hello world!"
```

## Gehri Jahaz:
String concatenation ke baare mein gehri jaankari ke liye, hum `printf()` aur `sprintf()` functions ka upyog kar sakte hai. Yeh humein formatting characters ka bhi pata lagane mein madad karta hai.
```
PHP
$name = "John";
$age = 30;
 
// Using `printf()` function
printf("My name is %s and I am %d years old.", $name, $age); // Outputs "My name is John and I am 30 years old."
 
// Using `sprintf()` function
$output = sprintf("My name is %s and I am %d years old.", $name, $age); // Stores the formatted string in a variable
echo $output; // Outputs "My name is John and I am 30 years old."
```

## Dekhiye Bhi:
Kisi bhi programming language ko seekhne ke liye practice hona bahut zaroori hai. Isliye, aap in links ko padhkar aur aur saare examples ko try karke apni string concatenation skills ko enhance kar sakte hai.
- [Official PHP string concatenation documentation](https://www.php.net/manual/en/language.operators.string.php)
- [TutorialsPoint PHP string concatenation tutorial](https://www.tutorialspoint.com/php/php_string_concatenation.htm)
- [W3Schools PHP string concatenation tutorial](https://www.w3schools.com/php/php_string_concat.asp)