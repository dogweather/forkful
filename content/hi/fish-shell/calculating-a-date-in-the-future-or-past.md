---
title:                "भविष्य या भूतकाल में एक दिनांक की गणना"
html_title:           "Fish Shell: भविष्य या भूतकाल में एक दिनांक की गणना"
simple_title:         "भविष्य या भूतकाल में एक दिनांक की गणना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyu

Sabse pehle, kya aapne kabhi future mein ya past mein kisi particular date ko calculate karne ki zarurat mehsoos ki hai? Fish Shell, aapki yeh zarurat ko poora karta hai aur aapko ek aasaan tareeke se specific date ko calculate karne ki suvidha deta hai. Aaiye, is article mein hum dekhte hai ki kaise aap Fish Shell ki madad se date ko future aur past mein calculate kar sakte hai.

## Kaise Kare

Fish Shell mein date ko future mein ya past mein calculate karne ke liye, aapko `date` command ka use karna hoga. Is command ke saath aap different flags bhi use kar sakte hai jo aapko desired date format mein output dega.

```
Fish Shell mein date calculate karna ka example:

```Fish Shell
date +%d-%m-%y
```
Output:
14-02-21

Yahan, humne `+%d-%m-%y` flag ka use kiya hai jo hume current date ko `DD-MM-YY` format mein deta hai. Aap apne hisab ke mutabik is command mein different flags use kar sakte hai.

## Deep Dive

Fish Shell mein date ko calculate karne ke liye, aapko `date` command ke saath kuch parameters bhi use karne hote hai. Kuch common parameters hai:`+%d` for date, `+%m` for month, `+%y` for year, `+%D` for complete date, `+%T` for complete time, etc.

Iske alawa, aap `--date` parameter ka bhi use kar sakte hai jisme aap specific date ko bhi enter kar sakte hai jaise `--date "5 days ago"` ya `--date "2 years from now"`. Yeh command apko specific date ke corresponding output dega.

## See Also
- [Fish Shell documentation] (https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell tutorial in Hindi] (https://www.geeksforgeeks.org/linux-fish-shell-tutorial-in-hindi/)