---
title:                "एक पाठ फ़ाइल को पढ़ना"
html_title:           "Fish Shell: एक पाठ फ़ाइल को पढ़ना"
simple_title:         "एक पाठ फ़ाइल को पढ़ना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun

Agar aap ek programmer hai aur *text file* ko padhna aur usme se data extract karna chahte hai, toh ye article aapke kaam ka ho sakta hai. Yahan hum Fish Shell ke madhyam se text file ko kaise padh sakte hai, iske baare mein jaanenge.

## Kaise Kare

```Fish Shell``` ke madhyam se text file ko read karne ke liye, pehle ```cat``` command ka use karna hoga. Iske baad aapko *file path* ko specify karna hoga jisme aapki text file stored hai. Ye command aapko text file ka pura content terminal par dikha dega.

```
cat /path/to/file.txt
```
Agar aapko sirf kuch specific data extract karna hai, toh aap ```grep``` command ka use kar sakte hai. Iske liye aapko ```cat``` command ke baad ```|``` (pipe) lagana hoga, phir ```grep``` command aur fir us data ka naam dena hoga jise aap extract karna chahte hai.

```
cat /path/to/file.txt | grep data
```
Is tarah aap text file se specific data extract kar sakte hai.

## Deep Dive

Fish Shell mein ek badiya feature hai jo hume text file ko direct terminal par read karne ki facility deta hai. Iske liye aapko *file path* ko terminal par drag and drop karna hoga, aur phir enter press karna hoga. Isse aapki text file terminal par automatically open ho jayegi.

## Dekhna Bhi

Agar aapko aur advanced text file read karne ke techniques jaanna hai, toh aap ye links check kar sakte hai:

1. [Fish Shell official website](https://fishshell.com/)
2. [Fish Shell tutorial](https://fishshell.com/docs/current/tutorial.html)
3. [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)

Main umeed karta hu ki aapko ye article helpful laga hoga. Happy coding!