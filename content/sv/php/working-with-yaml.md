---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett format för datautbyte, enkelt för människor att läsa och skriva. Programmerare använder YAML för konfigurationsfiler, datautbyte mellan språk och eftersom det är mer lättläst än XML eller JSON för komplex data.

## Så här gör du:
För att jobba med YAML-i PHP behöver du `yaml`-tillägget. Här är ett exempel på hur du läser och skriver YAML:

```PHP
<?php
// Förutsätter att yaml-tillägget är installerat
$yamlStr = "
en: Hello World
sv: Hej Världen
";

// Läsa YAML till en array
$data = yaml_parse($yamlStr);
print_r($data);

// Skapa en YAML-sträng
$array = ['en' => 'Goodbye World', 'sv' => 'Hej då Världen'];
$yaml = yaml_emit($array);
echo $yaml;
?>
```

Du bör se något liknande:

```PHP
Array
(
    [en] => Hello World
    [sv] => Hej Världen
)
en: Goodbye World
sv: Hej då Världen
```

## Fördjupning
YAML (YAML Ain't Markup Language) började användas tidigt på 2000-talet. Det står i kontrast till XML och JSON men behåller interoperabilitet. Tillägget `yaml` för PHP är inte standard, så det måste installeras manuellt. Alternativ för att jobba med YAML i PHP inkluderar också bibliotek som Symfony's Yaml-komponent.

## Se även
- Officiella YAML-webbplatsen: https://yaml.org
- PHP:s yaml-tillägg: https://pecl.php.net/package/yaml
- Symfony's Yaml-komponent: https://symfony.com/doc/current/components/yaml.html