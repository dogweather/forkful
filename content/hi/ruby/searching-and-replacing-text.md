---
title:                "टेक्स्ट को खोजना और बदलना"
html_title:           "Ruby: टेक्स्ट को खोजना और बदलना"
simple_title:         "टेक्स्ट को खोजना और बदलना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi programmer ke liye text ko search aur replace karna bahut important hai. Isse aap code ko optimize aur maintain karne mein madad milti hai, jisse aapko code ko likhne ke liye kam samay lagta hai.

## Kaise Karein

Ruby mein text ko search aur replace karne ka sabse aasan tareeka `gsub` method hai. Isme aapko do arguments dena hota hai - pahla argument wo text jo aap search karna chahte hai aur dusra argument wo text jo aap replace karna chahte hai.

```Ruby
sentence = "Mai Ruby programming ka article likh raha hoon."
replaced_sentence = sentence.gsub("programming", "karyakram")
puts replaced_sentence
```

Yeh code output ke roop mein "Mai Ruby karyakram ka article likh raha hoon." print karega.

Agar aapko search aur replace karne ke liye ek sentence mein se multiple words ko badalna hai, toh aap `gsub` method mein ek hash bhi de sakte hai. Ismein keys wo words honge jo aapko replace karna hai, aur values wo words honge jo aapko replace karna hai.

```Ruby
sentence = "Mai Ruby programming ka article likh raha hoon."
replaced_sentence = sentence.gsub({"programming" => "karyakram", "likh" => "likhta"})
puts replaced_sentence
```

Yeh code output ke roop mein "Mai Ruby karyakram ka article likhta raha hoon." print karega.

## Gehraai Se Jhaankein

`gsub` method ke alawa, dusre methods bhi hai Ruby mein jo text ko search aur replace karne mein madad karte hai. Kuch options hai jisse aap search ko case sensitive ya case insensitive bhi bana sakte hai. Text ko search karne ke liye regex bhi use kiya ja sakta hai.

See Also:

- [Ruby Docs on Gsub method](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Ruby Docs on Regex](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Ruby Monk's tutorial on replacing text](https://rubymonk.com/learning/books/1-ruby-primer/chapters/5-strings/lessons/45-replace)