---
title:                "स्ट्रिंग को लोअर केस में रूपांतरित करना"
html_title:           "Java: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Kyu: 

Shayad aapne kabhi socha ho ki kyu hum string ko lower case mein badalte hain. Lekin ye bahut hi zaroori hai kyunki lower case strings humare code ka consistency aur readability ko improve karta hai.

Kaise Kare: 

Toh chaliye ab dekhte hain ki hum kaise kisi bhi string ko lower case mein convert kar sakte hain. Iske liye hum `toLowerCase()` method ka istemal karenge jo String class mein available hai. Code ke madhyam se dekhein:

  ```Java
  String str = "HELLO WORLD";
  String lowerCaseStr = str.toLowerCase();
  System.out.println(lowerCaseStr);
  ```
Iska output hoga:
  ```Java
  hello world
  ```
Jaise ki hum dekh sakte hain, `toLowerCase()` method humare string ko pura lower case mein badal deta hai.

Gehri Jhaank: 

Agar hum baat karein string ke character encodings ki toh lower case characters ASCII (American Standard Code for Information Interchange) ke values ke sath aate hain. Ye values 97 se 122 tak hote hain. Isse hum ye samajh sakte hain ki lower case characters ka ASCII value upper case characters se kam hota hai. Isliye `toLowerCase()` method apne string ke har character ke ASCII value ko check karta hai aur agar ye upper case character ke range mein aa jata hai toh use lower case character mein convert karta hai.

See Also:

Agar aapko Java programming ke aur bhi articles padhne hain toh neeche diye gaye links ki madad se ve padh sakte hain:

- [Introduction to Java Programming Language](https://www.geeksforgeeks.org/introduction-to-java/)
- [Java String toLowerCase() method](https://www.geeksforgeeks.org/java-string-tolowercase-method/)
- [ASCII Table for Characters](https://www.asciitable.com/)