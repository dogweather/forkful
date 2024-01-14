---
title:    "PHP: स्ट्रिंग की लंबाई खोजना"
keywords: ["PHP"]
---

{{< edit_this_page >}}

"Kyu: String ki lambai dhundhna karne ka maksad"

Hamare aaj ke samay mein, programming ek bahut hi important skill ban gayi hai. PHP programming language har roop mein aaj ke samay ke sabse jyada use kiya jaane wala language hai. String ki lambai dhundhna, ek aise kaam hai jo ki har programmer ko aana chahiye. Is article mein hum aapko batayenge ki kyu aur kis tarah se aap PHP mein string ki lambai ko dhundh sakte hai.

"Kaise: String ki lambai dhundhne ka tarika"

PHP mein string ki lambai dhundhne ke liye hum "strlen()" function ka use karte hai. Is function mein hum ek string ka naam dete hai aur ye function hume us string ki lambai return karta hai. Neeche diye gaye code block mein humne ek example diya hai jisme humne "Welcome" string ki lambai dhundhi hai.

```PHP 
$string = "Welcome";
echo strlen($string); // Output: 7
```

Is code block mein "strlen()" function ka use kiya gaya hai jis se hume "Welcome" string ki lambai "7" return hui hai. Is tarah se hum koi bhi string ki lambai dhundh sakte hai.

"Deep Dive: String ki lambai ko samajhne ka gehra adhyayan"

Jab hum "strlen()" function ka use karte hai, toh hume kuch baato ka dhyan rakhna chahiye. Jaise ki is function mein bina string ka naam diye hum lambai nahi dhundh sakte hai. Is function mein kuch special characters ko count nahi kiya jata hai, jaise ki spaces, commas, etc. Isliye agar hum "Welcome!" ki lambai dhundhege toh output hume "11" return karega, kyuki isme 6 alphabets hai aur 5 special characters.

Isliye hume apne code mein string ke saare special characters ko count karne ke liye aur proper output lene ke liye code ko customize karna zaroori hai.

"Dekhiye Bhi:"

1. PHP String Functions List (https://www.w3schools.com/php/php_ref_string.asp)
2. Understanding strlen() function in PHP (https://www.php.net/manual/en/function.strlen.php)
3. String Functions in Hindi (https://www.geeksforgeeks.org/hindi/string-functions-in-php/)

**Dekhe Bhi**