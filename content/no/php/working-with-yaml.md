---
title:                "Å jobbe med yaml"
html_title:           "PHP: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML står for "YAML Ain't Markup Language" og er et tekstbasert språk som brukes til å skrive leselige datastrukturer. Det er spesielt nyttig for å representere konfigurasjonsdata og er ofte brukt i webutvikling.

## Slik gjør du det:
Det er enkelt å arbeide med YAML i PHP ved hjelp av noen innebygde funksjoner og klasser. Først må du inkludere yaml modulen ved å bruke funksjonen `yaml_parse()` og angi filen du vil lese. Deretter kan du bruke funksjonen `yaml_emit()` for å konvertere et PHP-array til YAML-format og skrive det til en fil.

```PHP
// Leser YAML-fil
$yaml_data = yaml_parse(file_get_contents("min_fil.yaml"));

// Skriver et PHP-array til YAML-fil
$my_array = array(
    "navn" => "Ole",
    "alder" => 25,
    "interesser" => array("programmering", "fotball", "reise")
);
$yaml_string = yaml_emit($my_array);
file_put_contents("ny_fil.yaml", $yaml_string);
```

## Dykk dypere:
YAML ble først introdusert i 2001 som et alternativ til XML og JSON for å skrive datastrukturer. Det er et populært valg blant utviklere på grunn av sin lesbarhet og fleksibilitet. Alternativene til å arbeide med YAML i PHP inkluderer å bruke biblioteker som Symfony YAML eller Spyc. Det er også verdt å merke seg at YAML har støtte for å inkludere andre YAML-filer og referere til variabler, noe som gjør det mulig å organisere store mengder data på en effektiv måte.

## Se også:
- [Dokumentasjon for yaml_parse()](https://www.php.net/manual/en/function.yaml-parse.php)
- [Dokumentasjon for yaml_emit()](https://www.php.net/manual/en/function.yaml-emit.php)
- [Symfony YAML bibliotek](https://symfony.com/doc/current/components/yaml.html)
- [Spyc bibliotek](https://github.com/mustangostang/spyc)