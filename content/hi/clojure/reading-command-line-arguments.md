---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
html_title:           "Clojure: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

"## Kya aur Kyun?"
Reading command line arguments ek aisi technique hai jo programmers istemal karte hai executable programs ke sath interaction karne ke liye. Command line arguments allows the user to provide input to a program at the time of execution, allowing for more versatile and customizable programs.

"## Kaise Karein?"
Hum command line arguments ko Clojure me kaise read kar sakte hai, iske liye do examples hain:

```Clojure
(defn greet [name]
  (println "Hello," name "!"))

;; Example 1:
(println "Please enter your name:")
(def my-name (read-line))
(greet my-name)

;; Example 2:
(defn- main []
  (println "Please enter your name:")
  (def my-name (read-line))
  (greet my-name))
(-main)
```

Pahle example me, hum user se input lete hai using the `read-line` function aur fir uss input ko `greet` function ke argument me pass karte hai. Dusra example me, hum `main` function ko `defn-` se define karte hain taki wo private ho aur direct call nahi kiya ja sake. Fir `main` function ko `(-main)` se run karte hai jisme hum `read-line` aur `greet` ka use karte hai.

"## Gehri Khumaari"
Command line arguments ko read karne ke liye hum shuru se se hi programming languages me use karte hai. Clojure me hum `read-line` ya `command-line-args` function ka use kar sakte hai, jinse hum command line arguments ko access kar sakte hai.

### Alternatives
Command line arguments ke alawa, hum programs ke sath user input le sakte hai using stdin/stdout, file reading, GUI, etc. Lekin command line arguments ki wajah se humehsa provide kiya jata hai aur programmers ke liye easy hota hai input lete wakt.

### Tafsili Talkhees
Command line arguments ko read karne se pahle, hume `argv` array ko parse karna padta hai. Clojure me `argv` array ki value `command-line-args` function se milti hai, jisko hum argument list ke sath pass kar sakte hai. Fir hum `#java/lang.System/getProperty` function ka use kar sakte hai, jo arguments ko `java.lang.System` class se padhne me help karta hai.

"## Dekhein se"
- [Command Line Arguments in Clojure](https://www.javatpoint.com/command-line-arguments-in-clojure)
- [Official Documentation for Clojure command-line-args](https://clojuredocs.org/clojure.core/command-line-args)