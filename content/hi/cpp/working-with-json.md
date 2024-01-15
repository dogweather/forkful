---
title:                "Json के साथ काम करना"
html_title:           "C++: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Kyon

Agar aap C++ ka upyog karke bhavishya mein data ko exchange karne ya stor karne mein interested hai toh aapko JSON ke baare mein jaankari hona zaroori hai. JSON ek simple aur flexible format hai jo web API, mobile apps aur databases mein data ko represent karne mein use kiya jaata hai. Iss article mein hum aapko batayenge ki C++ mein JSON ko kaise use kiya ja sakta hai.

## Kaise

```C++

#include <iostream> 
#include <json.hpp> 

int main() {
// Creating a JSON object 
nlohmann::json data = {
{"name", "John"},
{"age", 25},
{"gender", "male"}
};

// Accessing values from the object 
std::cout << "Name: " << data["name"] << std::endl;
std::cout << "Age: " << data["age"] << std::endl;
std::cout << "Gender: " << data["gender"] << std::endl;

// Adding new values to the object 
data["occupation"] = "engineer";

// Iterating through the object 
for (auto& element : data.items()) {
std::cout << element.key() << " - " << element.value() << std::endl;
}
}

```

Output:

```
Name: John
Age: 25
Gender: male
occupation - engineer
```

Iss example mein humne nlohmann ke JSON library ka upyog kiya hai. Iske alawa bhi aur libraries jaise ki RapidJSON aur JSON for Modern C++ bhi C++ mein JSON ka support deti hai. Is tarah se aap apne project ke requirements ke hisaab se library ka chunav kar sakte hai.

## Deep Dive

JSON (JavaScript Object Notation) ek standardized format hai jisse hum data ko encode aur decode kar sakte hai. Iss format mein data key-value pairs mein store kiya jaata hai. Iske alawa JSON objects, arrays, strings, numbers, boolean values aur null values ko bhi support karta hai. C++ mein JSON ka support third-party library ka upyog karke kiya ja sakta hai.

JSON ko C++ mein use karne ke liye, sabse pehle humein library ko download karke apne project mein add karna hoga. Iske baad hum JSON objects aur arrays ko create aur manipulate kar sakte hai. Hum kisi bhi value ko JSON format mein convert kar sakte hai aur isse other data structures mein convert bhi kar sakte hai.

See Also:

- [nlohmann/json](https://github.com/nlohmann/json)
- [MilaoToka/rapidjson](https://github.com/MilaoToka/rapidjson)
- [nlohmann/json Modern C++ JSON library](https://github.com/nlohmann/json)