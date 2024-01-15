---
title:                "टेक्स्ट को खोजें और प्रतिस्थापित करें"
html_title:           "Haskell: टेक्स्ट को खोजें और प्रतिस्थापित करें"
simple_title:         "टेक्स्ट को खोजें और प्रतिस्थापित करें"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi text ke andar kuch specific words ya patterns ko dhoondhna aur unhe badalna chaaha hai? Ye kuch common tasks hai jo hum log text processing mein karte hain. Haskell mein hum searching aur replacing ko bohot hi aasan aur powerful tarike se kar sakte hain. Chaliye dekhte hain kaise!

## Kaise Karein
```
Haskell mein, hum "replace" function ka use karke text mein pattern ko search aur replace kar sakte hain. Iss function mein hum do parameters pass karte hain - pehla parameter wo word ya pattern hai jo hum dhoondhna chahte hain aur dusra parameter wo word ya pattern hai jo usse replace karna hai.

```
Example:
```
replace "hello" "hi" "Hello world!" 
```
Output:
```
"Hi world!"
```
Isme humne "hello" ko "hi" ke sath replace kiya hai.

Agar hum ek se zyada words ko replace karna chahte hain, to hum multiple "replace" functions ka use kar sakte hain.
```
replace "hello" "hi" (replace "goodbye" "bye" "Hello and goodbye, world!")
```
Output:
```
"Hi and bye, world!"
```
Haskell mein hum "regex" ya regular expressions bhi use kar sakte hain searching aur replacing ke liye. Ye kiya jaata hai "regex-base" aur "regex-posix" packages ke through. Inke use se hum specific patterns ya characters ko bhi search aur replace kar sakte hain.

### Deep Dive
Searching aur replacing bohot sari programming languages mein available hai, par Haskell mein ye process bohot hi modular aur flexible hai. Isme hum functions mein parameters pass kar sakte hain aur multiple search aur replace ko bhi chain kar sakte hain. Regex ka use karke bhi hum specific aur complex patterns ko search aur replace kar sakte hain.

## See Also
- [Haskell Official Website](https://www.haskell.org/)
- [Haskell Tutorial - Search and Replace](https://www.tutorialspoint.com/haskell-string-replacement-functions)