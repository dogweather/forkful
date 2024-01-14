---
title:                "Haskell recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why 
YAML (YAML Ain't Markup Language) is a popular format for storing and exchanging data in human-readable form. It is commonly used for configuration files in software projects, making it an essential tool for developers. In this blog post, we will explore how to work with YAML in Haskell, a functional programming language.

## How To 
To start working with YAML in Haskell, we first need to install the "yaml" library using the following command in our terminal:
```Haskell
stack install yaml
```

Once the installation is complete, we can import the "Data.Yaml" module in our Haskell file using the following code:
```Haskell
import Data.Yaml
```

To read a YAML file, we can use the "decodeFile" function from the "Data.Yaml" module. Let's say we have a "config.yaml" file with the following content:
```yaml
name: John
age: 25
job: Developer
```

We can read this file and convert it into a Haskell value using the following code:
```Haskell
main = do 
  result <- decodeFile "config.yaml"
  case result of 
    Nothing -> putStrLn "Failed to read file."
    Just value -> putStrLn $ "Name: " ++ (value :: Value) .: "name" ++ "\nAge: " ++
                   (value :: Value) .: "age" ++ "\nJob: " ++ (value :: Value) .: "job"
```

The output of this code would be:
```
Name: John
Age: 25
Job: Developer
```

We can also write data into a YAML file using the "encodeFile" function. Let's say we have a Haskell value "user" with the following data:
```Haskell
user = User {username = "Jane", email = "jane@example.com", role = "Admin"}
```

We can write this data into a YAML file named "user.yaml" using the following code:
```Haskell
main = do
  encodeFile "user.yaml" user
```

The contents of the "user.yaml" file would be:
```yaml
username: Jane
email: jane@example.com
role: Admin
```

## Deep Dive 
Working with YAML in Haskell involves converting YAML data into Haskell values, and vice versa. This conversion is done using the "ToJSON" and "FromJSON" typeclasses, which provide methods for encoding and decoding data respectively. These typeclasses are implemented for most primitive and common data types in Haskell, making it easy to work with YAML files.

Some advanced features of working with YAML in Haskell include handling custom data types, combining multiple YAML files, and handling non-strict parsing.

## See Also 
- [Hackage: yaml package](http://hackage.haskell.org/package/yaml)
- [YAML specification](https://yaml.org/spec/1.2/spec.html)