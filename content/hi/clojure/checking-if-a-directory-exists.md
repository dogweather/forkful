---
title:                "प्रॉग्रामिंग पर लेख: डायरेक्टरी मौजूद है या नहीं जाँचें"
html_title:           "Clojure: प्रॉग्रामिंग पर लेख: डायरेक्टरी मौजूद है या नहीं जाँचें"
simple_title:         "प्रॉग्रामिंग पर लेख: डायरेक्टरी मौजूद है या नहीं जाँचें"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyu:
Yeh article directory ka existent/maujudgi ko check karna kyu important hai, iske pehle mai aapko batana chahta hoon ki hum kya karenge. Agar humare paas koi file ya folder exist karta hai, to hum usme changes ya modifications kar sakte hain. Lekin agar woh directory hi maujud na ho, to hume koi kaam nahi kar sakte. Isliye yeh janna bahut zaroori hai ki directory exist karta hai ya nahi.

## Kaise Kare:
Jaise ki maine bataya ki directory ki existent ko check karna bahut zaroori hai, ab mai aapko batata hoon ki hum isse kaise kare. Code examples aur sample output ke saath "```Clojure ... ```" code blocks mein coding karene ki koshish karein.

```
Clojure (mkdir "/path/to/directory")
```
Yeh code hume ek new directory create karna sikhta hai. Agar hume kisi specific directory ki existent ko check karna hai, to niche diye gaye code ko follow karein.

```
Clojure 
(if (.isDirectory (io/file "/path/to/directory")) true false)
```

Is code se hum directory ki existent ko check kar sakte hain. Agar directory exist karta hai, to output `true` hoga, warna `false` hoga.

## Deep Dive:
Directory ki existent ko check karne ke liye hum `io/file` function ka use karte hain. Ye function ek directory (folder) ya file ke path ko lekar naya ek file object banata hai.

```
(io/file "/path/to/file")
```

Is code se `file` object banta hai, jise hum phir `isDirectory` method ke saath use kar sakte hain. `isDirectory` method ek boolean value return karta hai, jo bataega ki file object ek directory hai ya nahi.

## See Also:
Agar aapko Clojure ke aur tutorials aur articles padhne hain, to aap in links par ja sakate hain:
- [Official Clojure website](https://clojure.org/)
- [Clojure Docs](https://clojure.org/index)
- [Learn Clojure in Y minutes](https://learnxinyminutes.com/docs/clojure/)