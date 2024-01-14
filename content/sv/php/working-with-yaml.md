---
title:                "PHP: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML har blivit alltmer populärt som format för att lagra data i webbutveckling. Detta är på grund av dess enkla syntax och läsbarhet för både människor och maskiner.

## Hur man gör

```PHP
// Ladda in YAML-fil
$yaml = yaml_parse_file("data.yml");
// Visa innehållet
var_dump($yaml);
```

````
OUTPUT:
array(3) {
  ["name"]=>
  string(11) "John Doe"
  ["age"]=>
  int(25)
  ["hobbies"]=>
  array(2) {
    [0]=>
    string(4) "hike"
    [1]=>
    string(6) "travel"
  }
}
````

## Djupdykning

YAML står för "YAML Ain't Markup Language" och anses vara en lättläst alternativ till JSON och XML. Det är också ett vanligt format för konfigurationsfiler i webbutveckling och används ofta för att konfigurera system och appar. En av fördelarna med YAML är att det kan ha flera nivåer av hierarki, vilket gör det idealiskt för komplexa datastrukturer.

## Se också

- [YAML officiell hemsida](https://yaml.org/)
- [Symfony YAML-komponent](https://symfony.com/doc/current/components/yaml.html)
- [Laravel YAML-paket](https://github.com/spatie/yaml)