---
date: 2024-01-30 18:57:34.014358-07:00
description: "In the world of Arduino, associative arrays let you pair keys with values,\
  \ kinda like how you'd match socks with their pairs. They're a go-to when you\u2026"
lastmod: '2024-03-13T22:45:00.315820-06:00'
model: gpt-4-0125-preview
summary: "In the world of Arduino, associative arrays let you pair keys with values,\
  \ kinda like how you'd match socks with their pairs. They're a go-to when you\u2026"
title: Using associative arrays
weight: 15
---

## What & Why?
In the world of Arduino, associative arrays let you pair keys with values, kinda like how you'd match socks with their pairs. They're a go-to when you need to store and retrieve data using descriptive names, making your code cleaner and way more understandable.

## How to:
Arduino, strictly speaking, doesn't have built-in support for associative arrays as you'd find in higher-level languages. But, fear not. We can get crafty using structures and arrays to mimic this functionality. Here's a simple example to create a basic "associative array" for storing and accessing temperatures for different cities.

First, define a structure to hold the city (key) and its temperature (value):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Next, initialize an array of `CityTemperature` objects:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Here's how you can access and display the temperature of a specific city:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("The temperature in Los Angeles is: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Nothing here for now.
}
```

Running this code would give you the output:

```
The temperature in Los Angeles is: 22.0
```

## Deep Dive
Historically, programming languages like C and C++ (from which Arduino syntax is derived) didn't come with built-in associative arrays, leading to workarounds like the one shown above. This approach is relatively simple but scales poorly as the data size increases due to its O(n) lookup time.

Languages such as Python offer dictionaries, and JavaScript has objects for this purpose, both of which are far more efficient for managing key-value pairs. In Arduino, when performance and efficiency become critical, developers might opt for more specialized data structures, like hash tables, implemented via libraries.

Although Arduino doesn't natively support associative arrays, the community has developed libraries like `HashMap` that can be added to your project to provide similar functionality with better performance than a DIY approach. These libraries typically offer a more elegant and efficient means of managing associative arrays, especially for more complex projects.
