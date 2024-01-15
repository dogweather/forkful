---
title:                "एक पैटर्न से मेल खाने वाले अक्षरों को हटाना।"
html_title:           "Clojure: एक पैटर्न से मेल खाने वाले अक्षरों को हटाना।"
simple_title:         "एक पैटर्न से मेल खाने वाले अक्षरों को हटाना।"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### Kyun:
Kya aap kabhi kabhi kisi dhang se likhe hue text mein se kuch characters ko hataana chahte hain? Yeh kisi specific pattern ke anusaar ho sakta hai, jaise ki numbers ya special characters. Iska ek common use case hai text editing aur data cleaning.

### Kaise Karain:
Pattern ke anusaar characters ko hataane ke liye, hum "```(remove pred coll)```" function ka istemaal kar sakte hain. Yeh function ek predicate aur ek collection lekar kaam karta hai. Predicate ek function hoti hai jo ek element ko lekar true ya false return karti hai. Aur collection uss text ke pieces ko contain karti hai, jinhe aap edit karna chahte hain.

Yeh function ek naya collection return karegi, jisme se original text ke pieces hata diye jaayenge, jo predicate ke liye true return karte hain. Yeh function ek sath multiple patterns bhi handle kar sakta hai.

```
;;; Text mein se numbers aur special characters hataana
(remove #(or (number? %) (char? %)) "H3ll0, w0rld!") ; Output: "Helloworld"
```

### Gaharaai Mein Jaaen:
Pattern matching aur text manipulation mein ek important concept hai regular expressions. Clojure mein hum inka istemaal "re-seq" aur "replace" functions ke through kar sakte hain. Iske alawa, hum "remove" function ke "filter" parameter ko bhi ek function ke roop mein define kar sakte hain.

Regular expressions ke liye "#" dhaatu ka istemaal karna hota hai aur isme "re-pattern" function ka upayog karna padta hai.

```
;;; Text mein se vowels hataana
(remove (filter #(#{"a" "e" "i" "o" "u"} %)) "Hello, world!") ; Output: "Hll, wrld!"
```

Aap upar diye gaye examples mein apne liye pattern aur functions define kar sakte hain aur apne specific use cases ke liye customize kar sakte hain.

### See Also:
- [Clojure String functions](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/match)
- [Clojure Regular Expressions](https://clojure.org/reference/regular_expressions)