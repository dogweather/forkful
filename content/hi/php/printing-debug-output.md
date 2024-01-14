---
title:    "PHP: डिबग आउटपुट प्रिंट करना"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyon
Debug output print karne ka karan hai ki yeh aapko code mein kisi bhi tarah ke error ya bug ko dhoondhne aur use fix karne mein madad karta hai.

## Kaise Kare
Agar aapko apne code mein kisi bug ko dhoondhna hai, toh aapko debug output print karna hai. Iske liye aapko `print_r()` ya `var_dump()` jaise functions ka upyog karna hoga. Iske baad aapko apne code mein un variables ko include karna hai jinhe aap debug output mein dekhna chahte hain.

```PHP
<?php
   $name = "Rohit";
   $age = 27;
   print_r($name);
   print_r($age);
?>
```
Output: Rohit27

## Gehra Jaanch
Debug output ko print karna kaafi faydemand hai kyunki isse aapko woh line of code pata chalti hai jahan par error ho raha hai. Aur yeh aapko code ki gehrai mein jaane aur errors ko find karne mein madad karta hai. Iske alawa, agar aapki website ya application slow chal rahi hai, toh aap debug output ke through performance issues ko bhi dhoondh sakte hain.

## Dekhein Bhi
[PHP documentation on var_dump()](https://www.php.net/manual/en/function.var-dump.php),
[PHP documentation on print_r()](https://www.php.net/manual/en/function.print-r.php)