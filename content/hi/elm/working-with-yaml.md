---
title:                "यामल के साथ काम करना"
html_title:           "Elm: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Kyun

YAML ke saath kaam karne ka karan ek simple aur lightweight programming language ka use karne se hai. Elm, jo ki functional programming paradigm par based hai, YAML files ko manipulate aur interact karne ke liye ek powerful tool hai.

## Kaise

```Elm
import YAML exposing (..)

ymlString = """
name: John Doe
age: 25
hobbies:
- reading
- coding
- hiking
"""

yml = parse ymlString

yml.name -- "John Doe"

yml.hobbies -- ["reading", "coding", "hiking"]

yml.age -- 25

ymlString' = toYaml yml
```

Iss code example mein humne Elm ke YAML library ko import kiya aur ek sample YAML string ko parse karke usse interact kiya. Yahaan par hum dekh sakte hai ki hum kis tarah se YAML properties aur unke values ko access aur manipulate kar sakte hai. Iske alawa, humne `toYaml` function ka use kiya hai jo YAML data ko string mein convert karta hai.

## Deep Dive

YAML ke saath kam karne mein aur bhi kai interesting aur advanced techniques hai, jaise ki YAML anchors, aliases, aur flow style notation. Elm ke YAML library mein bhi in features ka support hai jiski madad se hum YAML files ko even more efficient tarike se handle kar sakte hai. Iske alawa, Elm ke type system ki flexibility aur powerful error handling features hume YAML data ko validate aur transform karne mein bhi madad karte hai.

## Dekhiye Bhi

- [Elm Documentation](https://elm-lang.org/docs)
- [YAML Website](https://yaml.org/)
- [YAML Tutorials](https://www.tutorialspoint.com/yaml/)
- [Elm-YAML Github repository](https://github.com/shibukawa/elm-yaml)