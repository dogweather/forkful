---
title:                "Json के साथ काम करना"
html_title:           "Gleam: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Why:
Kya aap JSON ke saath kaam karna chahte hain? JSON ek simple aur shaktishali data format hai jo aapko apne application mein data ko store aur transmit karne mein madad karta hai. JSON ismein readable aur easily maintainable strings ka format hai jo APIs aur web services ke liye popular hai.

## How To:
Coding examples aur sample output ke saath ek step-by-step guide:

```Gleam
// Ek simple JSON object banayein
let data =

  // Object ke andar string, number, aur boolean values add karein
  {
    "name": "John Doe",
    "age": 25,
    "isFemale": false
  }

// JSON object ko convert karein string mein
let dataString = data|>Json.Encode.encode_pretty(2)

// Output: "{"name": "John Doe", "age": 25, "isFemale": false}"

// Agar aapko kisi key ki value access karni hai

// Object ke key se value retrieve karein
let name = data["name"]

// Agar aapko array ke andar multiple objects ka data store karna hai
// Apne array ke size ko pass karein aur uski value ko assign karein
let customerList: List(Json.Value) = List.repeat(3, data)

// Ab aap JSON ko utilize kar sakte hain apne applications mein!
```

## Deep Dive:
JSON ke alawa bhi kai data formats aate hain jaise XML, CSV, ya HTML. Lekin JSON ke saath kaam karna asaan aur flexible hai. JSON ko isliye popular choice mana jata hai kyunki ismein data ko easily retrieve aur manipulate kiya ja sakta hai. Iske alawa, kai programming languages mein iska support hota hai aur aap isko use karke data ko cross-platform transmit kar sakte hain. Agar aap JSON ke depth mein jaana chahte hain, to aap JSON Schema, JSON Pointer, aur JSON Web Tokens jaise concepts ko explore kar sakte hain.

## See Also:
Agar aapko JSON ke baare mein aur jaankari chahiye, to aap in links ko check kar sakte hain:
- JSON Tutorial: https://www.geeksforgeeks.org/json-tutorial/
- JSON Basics: https://www.w3schools.com/js/js_json.asp
- Working with JSON in Gleam: https://gleam.run/documentation/json/overview.html