---
title:                "Arbeta med yaml"
html_title:           "PHP: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Om du är en PHP-utvecklare och letar efter ett sätt att strukturera och lagra data på ett enkelt sätt, då är YAML något du bör ta en titt på. YAML är en textbaserad datamarkeringsspråk som är enkelt att läsa och förstå, vilket gör det idealiskt för att skapa konfigurationsfiler eller lagra data som behöver vara organiserad på ett hierarkiskt sätt.

## Hur man gör

För att använda YAML i PHP, behöver du först installera YAML-biblioteket. Detta kan göras med Composer genom att lägga till paketet "symfony/yaml" i ditt projekt. Efter att ha installerat biblioteket, kan du enkelt skapa och läsa in YAML-filer med hjälp av PHP-koden nedan:

```PHP
// Skapa en array med data
$data = ['name' => 'John Doe', 'age' => 30, 'address' => '123 Main Street'];

// Konvertera arrayen till YAML-format
$yaml = \Symfony\Component\Yaml\Yaml::dump($data);

// Skriv YAML till filen
file_put_contents('data.yml', $yaml);

// Läsa in YAML från filen och konvertera till array
$loadedData = \Symfony\Component\Yaml\Yaml::parseFile('data.yml');

// Skriv ut arrayen
print_r($loadedData);
```

### Output:

```
Array
(
    [name] => John Doe
    [age] => 30
    [address] => 123 Main Street
)
```

## Djupdykning

En av de fördelar med YAML är dess läsbarhet för människor. Detta beror på dess enkla och intuitiva syntax som liknar ett naturligt språk. Det finns också möjlighet att inkludera kommentarer i YAML-filer för att förklara och dokumentera data på ett tydligt sätt.

En annan användbar funktion i YAML är möjligheten att inkludera referenser. Detta gör det möjligt att återanvända data på flera platser i samma YAML-fil utan att behöva duplicera det. Referenser skrivs med ett ampersand-tecken (&) och återanvänds med ett stjärntecken (*).

## Se även

För mer information om hur man arbetar med YAML i PHP, se följande länkar:

- [Symfony YAML-komponenten](https://symfony.com/doc/current/components/yaml.html)
- [YAML-introduktion](https://rollbar.com/blog/yaml-introduction/)