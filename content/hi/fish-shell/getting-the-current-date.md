---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Fish Shell: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyun

 Soch rahe ho ki aap abhi kyun taareekh jaan na chahte hain? Shayad aapko apne code me current date ka use karna hai ya fir aapko pata karna hai ki aaj ka din konsa hai. Fish shell me date ko kaise retrieve karna hai, uske baare me hum aaj baat karenge.

## Kaise Kare

Fish Shell me current date ko jaan ne ke liye kuch simple steps follow karen:

1. Sabse pehle apne terminal me "fish" command type kar ke Fish shell ko launch kare. Agar aapke paas ye shell already installed hai to ye kaafi simple hoga, agar installed nahi hai to apne operating system ke hisab se installation kare.
2. Ab terminal me "date" command type kar ke enter kare. Ye command aapko current date and time ko display karegi. 
3. Yaha par aapko current date and time ka format (MM/DD/YY) dekhne milega. Agar aapko koi specific format chahiye to aap "date" command ke sath "-s" flag ka use kar sakte hai, jaise ki "date -s '+%d-%b-%Y'" is command ka output aapko current date ko "DD-Mon-YYYY" format me dikhayega.


**Fish Shell me date ko display karne ka code example:**

```Fish Shell
date
07/03/2021
date -s '+%d-%b-%Y'
03-Jul-2021
```

## Gehri Jankari

Agar aap date ko puri tarah se samajhna chahte hain to Fish Shell ke date command ke sath kuch important flags ke baare me jaan lena jaruri hai. In flags se aap date ko kisi bhi specific format me display kar sakte hain.

- **-u:** Is flag ka use Greenwich Mean Time (GMT) me date and time display karne ke liye kiya jata hai.
- **-d:** Is flag ka use naya date and time ko display karne ke liye kiya jata hai.
- **-s:** Is flag ka use current date and time me specific changes karne ke liye kiya jata hai, jaise ki humne pehle bhi bataya ki is flag ke sath hum kisi specific format ka use kar sakte hain.
- **-h:** Is flag ka use display karne wale date ko usi waqt ki local time zone ke hisab se convert karne ke liye kiya jata hai.

## See Also
Aap date ko retrieve karne ke liye kisi specific language me bhi coding kar sakte hain, jaise ki Python, Java, C++ etc.

- [Python Date and Time](https://www.programiz.com/python-programming/datetime)
- [Java Date and Time](https://www.geeksforgeeks.org/java-util-date-class-java/)
- [C++ Date and Time](https://www.tutorialspoint.com/cpp_standard_library/cpp_date_time.htm)