---
title:                "Att arbeta med json"
html_title:           "Ruby: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med JSON (JavaScript Object Notation) är en vanlig uppgift för utvecklare som arbetar med webbapplikationer. JSON är ett kompakt format för att överföra data mellan klient och server, vilket gör det lätt att hantera och läsa data i dina program.

## Så här gör du

För att arbeta med JSON i Ruby, behöver du först installera JSON-paketet. Öppna din terminal och kör följande kommando:

```ruby
gem install json
```

När paketet är installerat kan du börja använda vårt JSON bibliotek. Låt oss först skapa ett simpelt JSON-objekt som innehåller information om en produkt:

```ruby
require "json"

product = {
    name: "iPhone X",
    brand: "Apple",
    price: 999
}
```

För att konvertera detta objekt till JSON, använder vi metoden `to_json`:

```ruby
json_product = product.to_json

puts json_product
=> {"name":"iPhone X","brand":"Apple","price":999}
```

För att konvertera en JSON-sträng till ett Ruby-objekt, använder vi metoden `parse`:

```ruby
ruby_product = JSON.parse(json_product)

puts ruby_product["name"]
=> "iPhone X"
```

## Djupdykning

En viktig del av att arbeta med JSON är att förstå dess struktur. JSON består av nyckel-värde-par som är avgränsade av en kolon och separerade av kommatecken. Det kan ha flera nivåer av inbäddade objekt och arrays.

Här är ett exempel på ett mer komplex JSON-objekt:

```ruby
{
    "users": [
        {
            "id": 1,
            "name": "John Doe",
            "age": 25,
            "address": {
                "street": "123 Main Street",
                "city": "New York",
                "state": "NY"
            },
            "hobbies": ["coding", "reading", "traveling"]
        },
        {
            "id": 2,
            "name": "Jane Smith",
            "age": 30,
            "address": {
                "street": "456 Oak Street",
                "city": "Los Angeles",
                "state": "CA"
            },
            "hobbies": ["painting", "cooking", "hiking"]
        }
    ]
}
```

För att få åtkomst till informationen i detta objekt, kan vi använda index eller nycklar. Till exempel, om vi vill få åtkomst till namnen på varje användare, kan vi använda en `each`-loop:

```ruby
users = JSON.parse(json_users)

users["users"].each do |user|
    puts user["name"]
end
=> "John Doe"
=> "Jane Smith"
```

## Se även

- [Ruby JSON bibliotekets dokumentation](https://ruby-doc.org/stdlib-2.7.3/libdoc/json/rdoc/JSON.html)
- [En guide till JSON](https://www.json.org/json-sv.html)
- [En komplett guide till Ruby för nybörjare](https://www.codecademy.com/learn/learn-ruby)