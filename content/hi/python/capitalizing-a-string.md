---
title:                "स्ट्रिंग को अपरकेस में बदलें"
html_title:           "Python: स्ट्रिंग को अपरकेस में बदलें"
simple_title:         "स्ट्रिंग को अपरकेस में बदलें"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

Sthosh sabhi ne ho kiya hai ki kaise hum string ko capital letters mein likhte hain. Ye ek bahut hi aam kaam hai jo hum programmers ko aksar karna padta hai. Iske piche kaaran hai ki capital letters mein likhne se aapke code ko padhne aur samajhne mein aasani hoti hai. Isse samjhane mein bhi aapko zyada samay nahi lagta hai aur apka code bhi clean aur organized dikhta hai.

# How to:

Python mein string ko capital letters mein likhne ke liye, hum inbuilt function "upper()" ka use karte hain. Is function ko hum jo string par call karte hain, uss string ke saare characters ko capital letters mein convert kar deta hai. Ye kaafi simple aur efficient tareeka hai string ko capital karne ka.

Example:

```Python
string = "ye ek sample string hai"
print(string.upper())
```
Output:
YE EK SAMPLE STRING HAI

Agar aapko sirf string ka first character capital karna hai, toh aap "capitalize()" function ka use kar sakte hain.

Example:

```Python
string = "sample"
print(string.capitalize())
```
Output:
Sample

# Deep Dive:

Historical context mein, string capitalizing ek bahut crucial kaam tha jab humare paas caps lock keys nahi the aur hum keyboard se shift press karke capital letters likhte the. Aaj kal ye ek basic feature hai jo har programmer ko aata hai.

Agar aap string capitalizing ke liye inbuilt functions use nahi karna chahte hain, toh aap khud custom function bhi likh sakte hain. Iske liye aapko string ke har character ko check karna hoga aur agar woh small letter hai toh use capital letter mein change karna hoga. Ye process thodi si tedious hai lekin aapko zyada control deta hai.

Implementation details ki baat karein toh, Python string capitalizing operations immutable hote hain, matlab ki aap original string ko change nahi kar sakte hai. Iske bajaye, ek new string create karna padta hai jisme capital letters wali string store hoti hai. Ye ek important concept hai Python programming mein aur aapko samajhna hoga.

# See Also:

1. [Python Documentation on String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
2. [W3schools Tutorial on String Methods](https://www.w3schools.com/python/python_ref_string.asp)
3. [GeeksforGeeks Article on String Capitalizing](https://www.geeksforgeeks.org/python-string-capitalize/)

Ye thi string ko capital letters mein likhne ki basic information Python programming ke context mein. Ummeed hai ab aapko samajh aa gaya hoga ki ye kaam kyun aur kaise kiya jata hai. Happy coding!