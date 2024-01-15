---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "PHP: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyun
Aaj kal hum sab apne din ka schedule taiyaar karne ke liye laptop, phone, ya kisi dusre electronic device ka istemaal karte hai. In devices me current date ki jaankari lene ke liye PHP programming language ka upyog kiya jaata hai. Isse hum apne applications me tarik, samay, vaar, mahina, ya saal ko daal sakte hai. Aise me aaj hum janenge ki PHP me current date kaise paaya jaata hai.

## Kaise
Sabse pehle, hume ```date()``` function ka upyog karne ki zaroorat hai. Is function me hum ```d-m-Y``` date format ko specify kar sakte hai. Agar hume sirf current date ki tarik chahiye to hum ```date("d")``` ka upyog kar sakte hai. Agar hum current time ki jaankari lena chahte hai to hum ```date("h:i:sa")``` ka upyog kar sakte hai.

```PHP
$date = date("d-m-Y"); //output: 09-09-2020
$time = date("h:i:sa"); //output: 09:25:29pm
```

Agar hume current date me timezone ka bhi pata karna hai to hum ```date_default_timezone_set()``` function ka upyog kar sakte hai. Is function me hum ek string ki value pass karke apne desired timezone ko set kar sakte hai. Jaise ki, ```date_default_timezone_set("Asia/Kolkata")``` hume current time India ki timezone ke hisab se dikhayega.

## Deep Dive
PHP me current date lene ke liye hum ```date()``` function ke alawa bhi kai aur functions ka upyog kar sakte hai. Jaise ki ```getdate()```, jisme hum current date ki jaankari ko associative array ke roop me le sakte hai. Is function me hum day, month, year, time, aur timezone ke saath-saath aur bhi kai properties ko retrieve kar sakte hai.

```PHP
$current_date = getdate();
echo $current_date["mday"]; //output: 09 (current day)
echo $current_date["weekday"]; //output: Wednesday(current day of the week)
echo $current_date["month"]; //output: September (current month)
echo $current_date["year"]; //output: 2020 (current year)
```

Iske alawa, PHP me hum ```DateTime``` class ka bhi upyog kar sakte hai current date lene ke liye. Is class me hum apne desired format ke according current date ko display kar sakte hai.

```PHP
$current_date = new DateTime(); 
echo $current_date->format('d-m-Y'); //output: 09-09-2020
```

## See Also
1. [PHP date format guide](https://www.php.net/manual/en/function.date.php)
2. [PHP timezones](https://www.php.net/manual/en/timezones.php)
3. [DateTime class in PHP](https://www.php.net/manual/en/class.datetime.php)

Prakashit kinaron se bahar niklo, aur apne current date ko PHP ke madhyam se paaayega!