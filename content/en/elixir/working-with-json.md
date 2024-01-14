---
title:                "Elixir recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON (JavaScript Object Notation) is a widely used data format for exchanging information between web servers and applications. As such, having a good understanding of working with JSON in Elixir can greatly enhance a programmer's abilities and open up new opportunities for working with modern web technologies.

## How To
To start, we will need to install the ```Jason``` dependency in our project by adding it to our ```mix.exs``` file:
```
defp deps do
    [
        {:jason, "~> 1.0"}
    ]
end
```
We can then use the ```Jason``` library to encode and decode JSON data. Let's say we have the following JSON object:
```
{
    "name": "Jane Doe",
    "age": 25,
    "hobbies": ["hiking", "reading", "painting"]
}
```
To encode this into a string, we can use the ```Jason.encode!/1``` function:
```
Jason.encode!(%{
    name: "Jane Doe",
    age: 25,
    hobbies: ["hiking", "reading", "painting"]
})
```
The output would be:
```
"{\"name\":\"Jane Doe\",\"age\":25,\"hobbies\":[\"hiking\",\"reading\",\"painting\"]}"
```
To decode a JSON string into Elixir data, we can use the ```Jason.decode!/1``` function:
```
Jason.decode!("{\"name\":\"Jane Doe\",\"age\":25,\"hobbies\":[\"hiking\",\"reading\",\"painting\"]}")
```
The output would be:
```
%{
    "name" => "Jane Doe",
    "age" => 25,
    "hobbies" => ["hiking", "reading", "painting"]
}
```

## Deep Dive
One interesting feature of the ```Jason``` library is its ability to customize how JSON data is encoded and decoded. This can be useful when working with complex data structures or when you want more control over the output. For example, we can define a custom function to encode and decode a user's profile data:
```
defmodule Profile do
    defstruct [:name, :age, :hobbies]

    def encode(%Profile{name: name, age: age, hobbies: hobbies}) do
        %{
            "full_name" => name,
            "years" => age,
            "interests" => hobbies
        }
    end

    def decode(%{"full_name" => name, "years" => age, "interests" => hobbies}) do
        %Profile{
            name: name,
            age: age,
            hobbies: hobbies
        }
    end
end
```
We can then use this custom function in our encoding and decoding processes:
```
json = Profile.encode(%Profile{name: "Jane Doe", age: 25, hobbies: ["hiking", "reading", "painting"]})
# Output: %{"full_name" => "Jane Doe", "years" => 25, "interests" => ["hiking", "reading", "painting"]}

profile = Jason.decode!(json, as: Profile)
# Output: %Profile{name: "Jane Doe", age: 25, hobbies: ["hiking", "reading", "painting"]}
```

## See Also
- Jason documentation: https://hexdocs.pm/jason/1.1.1/readme.html
- Elixir JSON Cheat Sheet: https://devhints.io/elixir-json
- Phoenix Framework JSON API tutorial: https://phoenixframework.org/blog/build-a-json-api-with-phoenix