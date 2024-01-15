---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Javascript: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi socha hai ki aapka computer ya mobile phone aapko current date and time kaise batata hai? Yeh sab possible hai, kyunki hum Javascript ke madad se current date aur time ko retrieve kar sakte hain.

## Kaise Karein
```Javascript
let currentDate = new Date(); // create new Date object
console.log(currentDate); // output: Sun Jun 13 2021 19:24:43 GMT+0530 (India Standard Time)

// get current date in specific format
let day = currentDate. getDate(); // get day of the month
let month = currentDate.getMonth() + 1; // get month (returns value between 0-11, so adding 1)
let year = currentDate.getFullYear(); // get full year

console.log(`${day}-${month}-${year}`); // output: 13-6-2021
```

## Gehri Jhaank
Javascript mein current date ko retrieve karne ke liye hum `Date()` object ka upyog karte hain. Is object mein humein flexible methods diye jaate hain jaise `getDate()`, `getMonth()` aur `getFullYear()`, jinse hum specific date format mein current date ko access kar sakte hain.

Ek aur tarika hai current date ko retrieve karne ka, jahan hum ek existing date object ko modify kar sakte hain.

```Javascript
let currentDate = new Date();

// modify date object to a specific date
currentDate.setFullYear(2022); // change full year to 2022
currentDate.setMonth(0); // change month to January (0 index)

console.log(currentDate); // output: Sat Jan 13 2022 19:34:45 GMT+0530 (India Standard Time)
```

## Dekhie Humari Dusri Articles
Agar aapko Javascript mein aur bhi mazedaar aur kaam ki cheezein seekhni hai, toh neeche diye gaye links par click karein:

- [Javascript Variables kaise declare karte hain](https://www.example.com/variables)
- [Arrays kya hote hain aur javascript mein unka upyog kaise karein](https://www.example.com/arrays)
- [Functions kya hote hain aur javascript mein unka upyog kaise karein](https://www.example.com/functions)

## Dekhie Aage
Ab aapko pata hai ki hum Javascript mein current date ko kaise retrieve karte hain. Agar aap aur bhi mazedaar programming languages aur concepts seekhna chahte hain, toh aap hamare [blog](https://www.example.com/blog) aur [YouTube channel](https://www.example.com/youtube) par visit karein. Sukriya!