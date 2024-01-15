---
title:                "एक नए परियोजना की शुरुआत करना"
html_title:           "Fish Shell: एक नए परियोजना की शुरुआत करना"
simple_title:         "एक नए परियोजना की शुरुआत करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Agar aap ek programming project shuru karna chahte hain to Fish Shell ek acha option ho sakta hai. Ye aapko efficient aur streamlined coding experience provide karta hai, jisse aapka project jaldi se develop ho sakta hai aur aapko kam samay mein zyada kaam karne ki permission deta hai.

## How To
Fish Shell mein ek naya project shuru karne ke liye, aapko kuch simple steps follow karne honge:

```
Fish Shell mein naya project shuru karne ke liye, aapko pehle `mkdir` command ka use karke ek naya directory create karna hoga.
Uske baad, `cd` command se uss directory mein enter karein.
Ab `touch` command ka use karke ek naya file banayein, jisme aap apna code likhenge.
Finally, apne code ko run karne ke liye `./filename` command use karein.
```

Sample output:

```
mkdir my_project
cd my_project
touch main.py
./main.py
```

## Deep Dive
Fish Shell mein naya project shuru karte waqt, aapko kuch important considerations rakhne honge. Jaise ki aapko apne project ke liye sahi se syntax highlighting aur code completion setup karna hoga. Iske liye aap Fish Shell ke built-in options ka use kar sakte hain ya fir third-party plugins install kar sakte hain.

Ek aur important aspect hai virtual environment setup. Fish Shell mein, `virtualenv` command ka use karke aap aasani se ek virtual environment create kar sakte hain, jisse aapke project dependencies alag rehte hain aur aapke system dependencies ko affect nahi karte hain.

## See Also
- [Official Fish Shell Website](https://fishshell.com/)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Github Repository](https://github.com/fish-shell/fish-shell)