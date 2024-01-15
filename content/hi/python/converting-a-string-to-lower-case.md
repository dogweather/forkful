---
title:                "स्ट्रिंग को लोअर केस में रूपांतरण करना"
html_title:           "Python: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun
Kisi bhi programming bhasha mein, humein string (shreni) ko manipulate karna sikhna zaroori hai. Ek aise common manipulation technique hai string ko lower case mein convert karna. Isse humara code easier to read aur comprehend hojata hai aur humare saath bhi bahut se fayde hote hain. Is article mein hum aapko batayenge ki kis tarah se aap Python mein ek string ko lower case mein convert kar sakte hain.

## Kaise Kare
```python
# Example 1
string = "HELLO WORLD"
print(string.lower())
# Output: hello world

# Example 2
string = "HeLLO WoRLd"
print(string.lower())
# Output: hello world
```
Jaisa ki humne dekha, humein bas string variable ke naam ke baad "lower()" function ka use karna hota hai, jo humare string ko lower case mein convert kar deta hai.

## Deep Dive
Python mein strings immutable (change nahi ho sakti) hoti hain, isliye hum count and replace ka use nahi kar sakte hain unke case ko badalne ke liye. Iske instead, hum lower() function ka use karte hain jo ek copy banake original string ke case ko change karta hai. Ye case-insensitive hota hai, matlab upper case letters ko bhi lower case mein convert kar deta hai.

## Dekhein Bhi
- String methods in Python: https://www.w3schools.com/python/python_ref_string.asp
- Official Python documentation on string methods: https://docs.python.org/3/library/stdtypes.html#string-methods
- Lowercase string in Python: https://www.programiz.com/python-programming/methods/string/lower