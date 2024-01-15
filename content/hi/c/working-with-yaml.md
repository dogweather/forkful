---
title:                "यामल के साथ काम करना"
html_title:           "C: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Kyon
YAML ek bahut prachalit file format hai jise data ko store aur transmit karne ke liye istemal kiya jata hai. YAML samajhne aur likhne mein bahut asaan hai, isliye bahut se developers iska istemal karte hain.

## Kaise Kare
Agar aap C programming language mein YAML ka istemal karna chahte hain, toh aapko ek library install karni hogi jise YAML parser kehte hain. Iss library ko istemal karke aap ek YAML file read aur write kar sakte hain.

Ek basic YAML file kuch iss tarah dikhta hai:

```C
names:
  - John
  - Jane
  - Bob

ages:
  John: 25
  Jane: 30
  Bob: 35
```

Iss file mein `names` aur `ages` dono keys hain jinki values lists aur dictionaries hain. Iss tarah ke structured data ko YAML mein store karke access karna bahut asaan hai.

YAML parser library ka istemal karne ke liye, aapko sabse pehle isse include karna hoga:

```C
#include <yaml.h>
```

Iss ke baad, aapko `yaml_parser_t` aur `yaml_document_t` structures ko initialize karna hoga. Phir aapko `yaml_parser_initialize()` aur `yaml_parser_load()` functions ka istemal karke YAML file ko parse karna hoga. Iss process ke baad, aap YAML file ke saare data ko access kar sakte hain.

## Deep Dive
Agar aap YAML file ko padh karke usme se specific data ko extract karna chahte hain, toh aap `yaml_mapping_t` aur `yaml_scalar_t` structures ka istemal kar sakte hain. `yaml_mapping_t` structure se aap keys aur values ko access kar sakte hain, jabki `yaml_scalar_t` structure se aap particular values ko access kar sakte hain.

Iss tarah se aap C programming language mein YAML ka istemal kar sakte hain aur apne code ko organized aur readable bana sakte hain.

## Dekhein Bhi
- [YAML Official Website](https://yaml.org/)
- [YAML Parser Library for C](https://pyyaml.org/wiki/LibYAML)
- [YAML Tutorials for Beginners](https://www.tutorialspoint.com/yaml/)