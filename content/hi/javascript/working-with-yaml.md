---
title:                "yaml के साथ काम करना"
html_title:           "Javascript: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi apne code ko organize karne ke liye YAML ka istemal kiya hai? YAML (YAML Ain't Markup Language) ek simple aur flexible format hai, jise aap programming languages ke saath bhi use kar sakte hain. Isse aap apne data ko asaan tareeke se store aur access kar sakte hain.

## Kaise Karein
YAML ko Javascript mein use karne ke liye, aapko kuch simple steps follow karne honge:

1. Sabse pehle, aapko YAML library ko install karna hoga. Ye npm package manager ka istemal karke easily ho sakta hai. Terminal mein `npm install js-yaml` likhkar ye library install kar sakte hain.
2. Ab aap `require()` function ka istemal karke YAML library ko apne code mein import kar sakte hain.
3. Ab aap YAML ka object `yaml` banalein, jisme aap apni YAML file ko load kar sakte hain. Jaise: 
```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

let yamlFile = yaml.safeLoad(fs.readFileSync('file.yml', 'utf8'));
```
4. Aap `yamlFile` object ka istemal karke apne YAML data ko access kar sakte hain, jaise:
```Javascript
console.log(yamlFile.key); // "value"
```

## Gahrai Se Jaaein
Agar aapko YAML ke baare mein aur jaankari chahiye, toh aap `js-yaml` library ke official documentation aur GitHub repository par jaa sakte hain. Iske alawa, aap YAML ke Python, Ruby aur Java ke implementations ke baare mein bhi padh sakte hain.

## Dekhein Bhi
Yadi aapko YAML ke saath kaam karne mein aur madad chahiye, toh yeh links aapke kaam aa sakte hain:
- [js-yaml library documentation](https://www.npmjs.com/package/js-yaml)
- [js-yaml GitHub repository](https://github.com/nodeca/js-yaml)
- [YAML Wikipedia page](https://en.wikipedia.org/wiki/YAML)