---
title:                "नियमित अभिव्यक्तियों का उपयोग करना"
html_title:           "Haskell: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Regular Expressions: Kya Aur Kyun?
Regular Expressions ek programming concept hai jo string matching aur manipulation mein kaafi madad karta hai. Ye ek text pattern ko define karta hai jo ko use karke strings ke matching, searching, aur replacing ka kaam aasan banata hai. Programmers regular expressions ka upyog isliye karte hain kyunki ye code ko powerful bana deta hai aur kam samay mein jyada kaam karne mein madad karta hai.

## Kaise Karein:
`Haskell` mein regular expressions bahut asaani se use kiye ja sakte hain, iske liye `Text.Regex.TDFA` library ka upyog karein.
```
import Text.Regex.TDFA
```
Iss library mein `=~` operator ka use karke hum string ko regex pattern ke sath match kar sakte hain aur `=~~` operator se hum replace aur extraction ka kaam kar sakte hain. Ye operators `=~` aur `=~~` same tarah se kaam karte hain jaise `==` aur `=` karte hain, bas inmein variable ek string ho aur dusra ek regex pattern ho.

Regex ko miltiple lines se match karne ke liye ```=~//m``` ka upyog karein. Aur inki case sensitive behaviour ko control karne ke liye ```=~//i``` ka upyog karein. Ye library pure Haskell basis par kaam karti hai aur cross-platform hai, isliye aap isko asaani se apne code mein add kar sakte hain.

## Behetareen Jaankari:
Regex ka itihaas 1951 mein ek mathematician aur computer scientist, Stephen Kleene, ne develop kiya tha. Ye us time ke programs mein bahut kaam aata tha par dheere dheere developers ne iski importance ko samjha aur use karne lage. Aaj bhi regex ek popular aur widely used concept hai, aur kayi languages jaise Python, Java, aur JavaScript mein bhi iska upyog hota hai.

Regex ke alawa bhi string matching aur manipulation ke liye kayi alternative techniques hain, jaise ki string functions, pattern matching, aur parsing. Par regex ka upyog karna code ko concise aur efficient banata hai, especially jab hume complex string patterns ko manipulate karna hota hai.

Regular expressions implement karne ke liye kayi algorithms aur techniques hain, aur har language mein iski implementation alag alag ho sakti hai. Isliye jab aap regex ka upyog karte hain, aapko language specific libraries aur implementions ko dhyaan mein rakhna chahiye.

## Zyaada Jaankari ke Liye:
Agar aapko regular expressions ke baare mein aur jaankari chahiye to aap iske official documentation aur tutorials par refer kar sakte hain. Aur agar aapko specific languages jaise Python ya Java mein regex ka upyog karna hai, to aap us language ke official documentation aur tutorials par bhi refer kar sakte hain.

Ab aap regular expressions ke basic concept aur implementation ko samajh gaye hain, to aap isse apne programming skills ko improve karne mein use kar sakte hain. All the best!