---
title:                "अलगाव को निचले अनुमति में परिवर्तित करना"
html_title:           "PHP: अलगाव को निचले अनुमति में परिवर्तित करना"
simple_title:         "अलगाव को निचले अनुमति में परिवर्तित करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi programming language mein, string manipulation ek bahut important aspect hai. String ka manipulation karne ke liye, hume kai alag alag functions aur methods ka use karna padhta hai. Ek common scenario hai jahan hume ek string ko lower case mein convert karna hai. Iska karan ho sakta hai ke hume input string mein se case sensitivity ko hatakar, usse compare karna ho ya phir humare code mein lower case strings ka use hona ho. Is article mein hum dekhenge PHP mein string ko lower case mein convert kaise karte hai aur iska kya faayda ho sakta hai.

## Kaise Kare

String ko lower case mein convert karne ka sabse easy aur direct tareeka hai PHP mein `strtolower()` function ka use karna. Is function ke andar hume bas woh string deni hoti hai jise hum lower case mein convert karna chahte hai. Iske baad function hume ek naya string return karta hai jise lower case mein convert kiya jaata hai.

```PHP
$input = "HELLO WORLD";
$output = strtolower($input);
echo $output;
```

Output: hello world

Is coding example mein humne `strtolower()` function ka use kiya hai ek input string `HELLO WORLD` ko lower case mein convert karne ke liye. Isse hume `hello world` output mila hai.

## Deep Dive

PHP mein string ko lower case mein convert karne ke liye hume `mb_strtolower()` function ka bhi use kar sakte hai. Yeh function UTF-8 characters ko bhi sahi tarah se handle karta hai. Is function ke use ka syntax `mb_strtolower(string $string [, string $encoding = null])` hai.

Kuch aur methods mein string ko lower case mein convert karne ke liye `strtoupper()` aur `mb_convert_case()` ka use kiya jaata hai. Inme se `strtoupper()` function upper case mein convert karta hai ek string ko aur `mb_convert_case()` function upper ya lower case mein convert karne ke liye `MB_CASE_UPPER` aur `MB_CASE_LOWER` constant ka use karta hai.

## See Also

- [strtolower() function documentation](https://www.php.net/manual/en/function.strtolower.php)
- [mb_strtolower() function documentation](https://www.php.net/manual/en/function.mb-strtolower.php)
- [strtoupper() function documentation](https://www.php.net/manual/en/function.strtoupper.php)
- [mb_convert_case() function documentation](https://www.php.net/manual/en/function.mb-convert-case.php)