---
title:                "तारीख को स्ट्रिंग में रूपांतरित करना"
html_title:           "Bash: तारीख को स्ट्रिंग में रूपांतरित करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyu: Date ko string mein convert karne ke kaaran
Date ki tarikh ya samay ko string mein badalna ek kaafi common task hai jab hum Bash programming language mein kaam karte hain. Time stamp aur date ki formatting kayi baar alag alag tariko se ki jaati hai aur aapko apne project ke requirements ke according date ko string mein badalna pad sakta hai.

## Kaise Kare: 
Date ko string mein convert karne ke liye, hum `date` command ka istemal karenge. Yeh command system date aur time ko print karne ke liye istemal hoti hai. Ismein `+` sign ka istemal karke hum date ke format ko customize kar sakte hain. Neeche diye gaye code snippet mein aapko ek simple example mil jayega:

```Bash
date +"%m/%d/%Y"
```
Is command ko run karne ke baad, aapki screen mein current date ke format mein ek string printed hoga. Is tarah se aap apne hisab se date ki formatting kar sakte hain. Agar aap chahte hain ki output mein time bhi include ho, toh is tarah ka command istemal kar sakte hain:

```Bash
date +"%m/%d/%Y %H:%M:%S"
```

### Advanced Example:
Agar aap chahte hain ki date ko unix timestamp format mein print kare, toh aap `date` command ke saath `+%s` option ka istemal kar sakte hain. Yeh option aapko current date ko seconds mein return karega. Is tarah se:

```Bash
date +%s
```

Is tarah se aap apne project ke requirements ke according date ko string mein convert kar sakte hain.

## Deep Dive:
Date ko string mein convert karne ke liye, `date` command ke alawa bhi kayi aur options hain jaise ki `strftime`. Yeh option aapko date ko specific format mein print karne ki flexibility deta hai. Aap `man date` command ko istemal karke iske aur bhi options ke bare mein jaan sakte hain.

## See Also:
- [Bash scripting tutorials](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Date command documentation](https://man7.org/linux/man-pages/man1/date.1.html)
- [Stack Overflow discussion on converting date to string in Bash](https://stackoverflow.com/questions/6105820/converting-current-date-into-string)