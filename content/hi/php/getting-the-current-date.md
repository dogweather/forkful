---
title:    "PHP: वर्तमान तारीख प्राप्त करना"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyu
Agar aap ek PHP programmer hai, to aapne shayad kabhi na kabhi is sawal ka jawab dhundha hoga ki "abhi kaunsa tarikh hai?" Ya shayad aap ek website ya application bana rahe hai jisme aapko samay aur tithi ka pata hona zaruri hai. Isliye, "abhi ki tarikh" ka pata hona bahut zaruri hai har ek programmer ke liye.

## Kaise Kare
Agar aap PHP mein "abhi ki tarikh" jaanna chahte hai to aapko bas ek simple function ka use karna hoga: `date()`. Is function ke andar aapko 1 parameter dena hoga jisse aap apne hisab se tarikh ko format kar sakte hai. Man lijiye aapki website mein aapko sirf tarikh ke din aur mahine ka pata hona hai, to aap is tarah se function ka use kar sakte hai:

```PHP
<?php
$date = date("d-m");
echo $date;
```
Is code ke output mein aapko tarikh ke din aur mahine ka format mil jayega. Agar aapko samay bhi pata karna hai to aap `h-i-s` format ka use kar sakte hai. Aur agar aapko ek specific tithi ki jankari chaahiye to aap `date()` ke saath `strtotime()` bhi use kar sakte hai.

## Gehri Jankari
Agar aapko "abhi ki tarikh" ke bare mein gehri jankari chaahiye, to aapko pata hona chahiye ki PHP mein `date()` function ka use UNIX timestamp ke saath hota hai. Is feature ki wajah se aap apni website mein local time ke alawa kisi doosre desh ya time zone ki tarikh bhi show kar sakte hai. Iske alawa, PHP mein ek aur function hai `mktime()` jo aapko kaafi flexibility deta hai tithi aur samay ko customize karne mein.

## Dekhe Bhi
Agar aapne abhi tak PHP ke date functions ke baare mein gehri jankari nahi li hai, to aapko iske saath saath [`strtotime()`](https://www.php.net/manual/en/function.strtotime.php) function ka bhi use sikhna chahiye. Aur agar aapko aur bhi advanced date functions aur concepts ke baare mein jaanna hai to aap [PHP date and time functions documentation](https://www.php.net/manual/en/ref.datetime.php) ko dekh sakte hai.