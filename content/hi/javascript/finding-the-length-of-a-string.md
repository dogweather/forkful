---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Javascript: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Kyun

Ek aksar pucha jane wala sawal hai ki kisi bhi programming language mein string (shabd) ki lambai jaanana kyon zaroori hai? Iska jawab bahut simple hai - string lambai ko jaanane se hum apne code mein sahi tarah se kaam kar sakte hain aur sahi output hasil kar sakte hain. String lambai ko jaanana ek basic concept hai jo har programmer ko aana chahiye.

# Kaise karein

Kisi bhi string ki lambai ko jaanane ke liye, hum Javascript ke built-in function "length" ka istemal kar sakte hain. Is function ko use karne ke liye, hum simply dot (.) operator ka istemal karenge aur uske baad "length" likhenge. Iske baad hum pehle string ka naam likhein aur uske baad "length" function ko call karein.

```Javascript
var name = "Hindi readers";
console.log(name.length);
```
Is code mein, "name" variable mein "Hindi readers" string assign kiya gaya hai aur phir console.log() function ki madad se uski length ko print kiya gaya hai. Yeh code output mein 13 dikhaega, kyun ki "Hindi readers" string mein 13 characters hote hain.

# Gehri Jhalak

String ki lambai ko jaanane ke peeche gehre concepts hain. Sabse pehle, humein yeh jaanna zaroori hai ki string ko computer mein binary code mein store kiya jata hai, jahan har character ko "0" aur "1" ko represent kiya jaata hai. Har character ka binary code mein ek fixed length hota hai, jis se string ki lambai bhi fixed ho jaati hai.

Iske alawa, har programming language mein string ki length alag tarike se calculate kari jaati hai. Kuch languages mein string mein spaces aur special characters ko bhi consider kiya jaata hai jab ki kuch mein sirf alphabets aur numbers ko hi count kiya jaata hai.

Yeh jaankari aapko programming mein aage badhne mein madad karegi, kyun ki aapko har language mein string ki length ko samajhna zaroori hai.

# Dekhein Bhi

- [Javascript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [GeeksforGeeks - length() method in Javascript](https://www.geeksforgeeks.org/javascript-string-length/)
- [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)