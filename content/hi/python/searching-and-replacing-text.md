---
title:                "पाठ को खोजना और प्रतिस्थापन करना"
html_title:           "Python: पाठ को खोजना और प्रतिस्थापन करना"
simple_title:         "पाठ को खोजना और प्रतिस्थापन करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Kya aapne kabhi text ko dhoondhne aur badlne ke liye bahut saare documents ko haath se edit karna pada hai? Yeh bahut time-consuming aur exhausting ho sakta hai. Isiliye, Python mai text searching aur replacing ke feature ka istemal karke aap apne documents ko asaani se edit kar sakte hain.

## How To

```Python
# Sabse pehle, aapko apne document ko open karna hoga.
file = open('document.txt', 'r')

# Fir aapko woh string ya word jo aap replace karna chahte hain, specify karna hoga.
search_string = "lorem"

# Iske baad, woh string ya word jisse aap replace karna chahte hain, specify karna hoga.
replace_string = "ipsum"

# Ab hum `search_string` ko `replace_string` se replace karenge.
document = file.read().replace(search_string, replace_string)

# Humne `document` variable ko update kar liya hai, ab hum usse wapas se file mai save karenge.
file = open('document.txt', 'w')
file.write(document)
file.close()

# Aap dekh sakte hain ke `lorem` ab `ipsum` mai convert ho gaya hai.
```
Output: Yeh bahut **saral** hai!

## Deep Dive

Aap dekh sakte hain ke humne `replace()` method ko use kar ke kaise text ko replace kiya hai. Yeh method string data type ki built-in method hai aur isse hum string mai se ek specific substring ko dusre substring se replace kar sakte hain.

Humne yaha `r` mode ka istemal kiya hai `open()` function mai. Yeh mode file ko read karne ke liye hai. Iske alawa, humne `w` mode ka  istemal kiya hai file ko write karne ke liye. Agar aapne kisi dusri mode ka istemal kiya hai, toh aapko `r` mode se `w` mode mai switch karna hoga.

## See Also

- [Python String methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Python File handling](https://www.geeksforgeeks.org/file-handling-python/)
- [Regular expressions in Python](https://www.programiz.com/python-programming/regex)