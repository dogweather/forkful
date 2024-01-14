---
title:                "PHP: दो दिनांकों की तुलना करना"
simple_title:         "दो दिनांकों की तुलना करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyu
Aaj ke samay mein, bahut saare web applications aur websites par tithi aur samay ka mukabla karna aam hai. Yeh ek mahatavapurna kary hai jo humein apne projects mein karne ki zaroorat padti hai. PHP mein, do dates ko compare karne ka kaam karna behad hi aasan hai. Is blog post mein hum dekhenge ki do dates ko compare karna kis tarah se sambhav hai aur isse humein kya fayda hoga.

## Kaise
Agar aap PHP mein do dates ko compare karna chahte hai toh apko sirf *date_diff* function ka upyog karna hoga. Is function ke through hum do dates ko compare kar sakte hai aur pata laga sakte hai ki kitne din, mahine, saal ya fir ghante, minute, second tak ka antar hai. Niche diye gye code blocks mein humein yeh function istemaal karna sikhaya jayega:

```PHP
// Humne 2 dates banaye hai
$date1 = date_create('2021-01-01');
$date2 = date_create('2021-02-01');
// Humne date_diff function ka upyog kiya aur output ko $diff variable mein store kiya
$diff = date_diff($date1, $date2);
// Ab humein output print karna hai
echo $diff->format('%R%a days');
```

Output:

+31 days


Is tarah se hum 2 dates ko compare karke output le sakte hai. Iske alawa hum *strtotime* function bhi istemaal kar sakte hai. Is function ke through hum ek date ko dusre format mein convert kar sakte hai jaise ki *Y-m-d* se *F j, Y*.

## Gehri Jankari
Dates ko compare karna ek bahut hi zaroori functionality hai jise hum apne projects mein istemaal karte hai. Isse hum apne users ko accurate information provide kar sakte hai aur unhe events aur tithi ki sahi jaankari de sakte hai. PHP mein, do dates ko compare karne ke liye bahut se functions available hai jaise ki *date_diff*, *strtotime*, *mktime* aur *cal_days_in_month*.

Do dates ko compare karte waqt humein thodi saavdhani rakhni chahiye kyunki kai baar date format ki wajah se errors aa sakte hai. Isliye humein date format ko bhi sahi se samjhna zaroori hai. Ek baar hum acchi tarah se date functions ke bare mein jaan le, tab hum bahut se complex tasks ko bhi aasaani se kar sakte hai.

## Dekhiye Bhi
Agar aap PHP programming ke baare mein aur bhi gehri jankari lena chahte hai toh niche diye gye links ko jarur check kare:

- [PHP Date Functions](https://www.php.net/manual/en/function.date.php)
- [PHP Date Formats](https://www.php.net/manual/en/datetime.format.php)
- [Difference between two dates in PHP](https://www.geeksforgeeks.org/php-difference-between-two-dates/)
- [PHP Programming Tutorials for Beginners (Hindi)](https://www.guru99.com/php-tutorials.html)

Asha karte hai ki aapko yeh blog post pasand aaya hoga aur aap ab acchi tarah se dates ko compare kar payenge. Dhanyavaad!