---
title:    "Fish Shell: पैटर्न से मेल खाते अक्षरों को हटाना"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyun
Kya aap kabhi sochte hain ki aapka Fish Shell command prompt cluttered ho gaya hai aur aapko unwanted characters ka saamna karna pad raha hai? Kya aap apne command prompt ko saaf aur organised rakhna chahte hain? Iss situation mein, aap delete characters matching a pattern ka istemaal kar sakte hain. Isse aapko apne command prompt mein keval zaroori aur desired characters hi dikhai denge, jisse aapko kaam karne mein aasani hogi.

## Kaise Karein
```Fish Shell mein characters matching a pattern delete karne ke liye, aapko yeh steps follow karne honge:
1. Pehle, aapko command prompt mein yeh command likhni hogi: `set -U fish_key_bindings fish_delete_char
2. Ab aapko woh pattern ya characters batana hoga jo aap delete karna chahte hain. Iske liye, aap yeh command type kar sakte hain: `bind \cD delete-or-exit-visual-mode`.
3. Iske baad, aapka cursor pattern ya character ke paas jaega aur use highlight karega.
4. Jab aap `\cD` dabayenge, woh character ya pattern delete ho jaega.

Iss tarah se, aap asaani se characters matching a pattern delete kar sakte hain Fish Shell mein.

## Deep Dive
Delete characters matching a pattern ka istemaal karte waqt, aap kisi bhi specific character ya pattern ko delete kar sakte hain. Isse aap apne command prompt ko saaf aur organised rakh sakte hain. Iske alawa, aap `\cD` ki jagah `\cW` bhi use kar sakte hain jisse cursor sirf ek word forward jaega. Isse aapko kaafi time aur energy bachega.

## Dekhein Bhi
**See Also:**

1. [Fish Shell Command Line Editing](https://fishshell.com/docs/current/cmdline.html)
2. [Deleting Characters in Emacs Mode](https://fishshell.com/docs/current/cmdline.html#emacs-deleting-characters)
3. [Deleting in Vi Mode](https://fishshell.com/docs/current/cmdline.html#vi-ins-mode-deleting)
4. [Fish Shell User Guide](https://fishshell.com/docs/current/index.html)