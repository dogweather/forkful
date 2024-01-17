---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Gleam: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Kya aur Kyon?

Capitalizing ek tarika hai jisme ham ek string ke har word ko upper case mein change karte hai. Programmers isse karte hai taaki string ko easily padha ja sake aur usme consistency bani rahe.

# Kaise Kare?

Gleam mein, hum `String.capitalize` function ka istemaal kar sakte hai. Iske liye hum ek string ko function ke andar pass karenge aur upper case mein ho jaane ke baad wo string return karega. Iske alawa, hum `String.to_uppercase` function bhi use kar sakte hai jo same kaam karta hai lekin uppercase mein puri string ko return karta hai.

```
Gleam.run()
import gleam/string

let str = "mera naam swati hai"
gleam/string.String.capitalize(str)

// Output: "Mera Naam Swati Hai"
```

# Gehri Jhaank

Capitalizing ka concept asal mein English language se aaya hai jaha jaise hum proper nouns jaise naam ko upper case mein likhte hai, waise hi programmers bhi apne code mein consistency maintain karne ke liye ye technique istemaal karte hai. Iske alawa, dusre programming languages mein bhi `capitalize` function ki alag alag variations hoti hai, jaise ki keval pehle word ko capitaliz