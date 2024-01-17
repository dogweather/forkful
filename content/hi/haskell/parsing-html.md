---
title:                "एचटीएमएल की टालना"
html_title:           "Haskell: एचटीएमएल की टालना"
simple_title:         "एचटीएमएल की टालना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Kya aur Kyun?
Parsing HTML kya hai? Yeh ek tarika hai website se text data ko extract karne ka. Iss process mein, ek HTML document ko parse karte hue, alag-alag elements, tags aur attributes ko alag karke data ko sahi tarike se display kia jata hai. Programmers iss kaam ko karne ka reason yeh hota hai ke unnko specific website se data collect karna hota hai aur yeh sahi tarike se upar se karna possible nahi hota hai.

## Kaise Karein:
```Haskell
import Text.HTML.TagSoup

-- HTML document ko fetch karein
html <- openURL "https://www.examplewebsite.com"

-- HTML ko parse karein
tags <- parseTags html

-- Specific tag aur uske content ko extract karein
let headers = sections (~== "<h1>") tags
print headers
```

Yeh code example mein, Text.HTML.TagSoup library use kiya gaya hai jo ki ek popular library hai Haskell mein HTML parsing ke liye. Iss example mein, ek website se HTML document ko fetch karna aur usko parse karna dikhaaya gaya hai. Phir specific tag ko identify karke uske content ko extract kiya gaya hai.

## Gehre Khulase:
Parsing HTML purane se purane tasks mein se ek hai jo ki internet ke shuruwat se hi developers ke liye ek daunting task raha hai. Phir bhi, aajkal iss process ko aur bhi easy banane ke liye kuch alternatives available hai jaise ki kuch IDEs, libraries, aur tools jo ki HTML parsing ko automated aur efficient bana dete hai.

Iss process mein ek important cheez yeh hoti hai ki HTML document mein errors ho sakte hai aur inn errors ko handle karna important hota hai. Haskell mein, ```parseTags``` function iss kaam ko sambhalta hai aur automatically errors ko handle karke sahi output deta hai.

## Dekhein Bhi:
- [Haskell TagSoup Library](https://hackage.haskell.org/package/tagsoup)
- [Haskell Web Scraping Tutorial](https://www.youtube.com/watch?v=4DIpB-bXnso)
- [Haskell for Web Development](https://www.haskell.org/haskellwiki/Haskell_for_Web_Development)