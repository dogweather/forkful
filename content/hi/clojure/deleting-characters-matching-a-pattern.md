---
title:                "Clojure: पैटर्न को मिलाने वाले अक्षरों को हटाना"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyon

Kabhi kabhi hume ek pattarn se milta julta character hataana hota hai, jaise ki email addresses mein se "@" ko hataana ya HTML tags ko hataana apne string se.

## Kaise Kare

Iss situation mein, hum `clojure.string/replace` function ka use kar sakte hain jo humare string mein se humare dwara specify kiye gaye character ya pattern ko hata deta hai. Yeh function teen parameters leta hai - original string, pattern, aur replacement string. Dusron ke sath alag hai, replacement string mein koi character replace nahi kiya jata, yeh sirf match huye characters ko hataata hai.

```
;;; `clojure.string/replace` ka example
(clojure.string/replace "Hey, this is my email: example@example.com" #"@" "")  ;=> "Hey, this is my email: exampleexample.com"

(clojure.string/replace "This is a <strong>bold</strong> statement." #"<[^>]+>" "") ;=> "This is a bold statement."
```

## Gehri Jhaank

Yeh function `replace` function par based hai jo `java.util.regex.Matcher` object ka use karke matches ko replace karta hai. Yadi aap replacement string mein regex groups ka use karna chahte hain, toh aap `"$1"` ki tarah use kar sakte hain, jaha `$1` pehla group hota hai aur `$2` dusra group.

Iske alawa, agar hume sirf match huye characters ko replace karna hai aur replacement string nahi, toh hum `""` ki jagah `nil` bhi use kar sakte hain.

## Dekhein Bhi

- [Clojure.org - String Functions](https://clojure.org/reference/java_interop#_string_functions)
- [ClojureDocs - String](https://clojuredocs.org/clojure.core/string)