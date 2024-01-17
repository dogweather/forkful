---
title:                "Arbeta med yaml"
html_title:           "Elm: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML står för "YAML Ain't Markup Language" och det är ett format för att strukturera data på ett läsbart och enkelt sätt. Programmörer använder YAML för att läsa och skriva data på ett effektivt sätt, speciellt inom webbutveckling och konfigurationshantering.

## Så här:
Ett vanligt användningsområde för YAML inom Elm är för att definiera en lista av objekt. Till exempel:

```
listeAvObjekt : List { namn : String, ålder : Int }
listeAvObjekt =
    [ { namn = "Anna", ålder = 30 }
    , { namn = "Peter", ålder = 25 }
    , { namn = "Maria", ålder = 27}
    ]
```

YAML tillåter oss att strukturera denna data på ett lättläst sätt:

```
- namn: "Anna"
  ålder: 30
- namn: "Peter"
  ålder: 25
- namn: "Maria"
  ålder: 27
```

## Djupdykning:
YAML utvecklades av Clark Evans och Ingy döt Net och först släpptes år 2001. Det används ofta för att skapa konfigurationsfiler för webbapplikationer, men det används även för andra ändamål som att strukturera data för API-anrop. En annan populär metod för att strukturera data är JSON (JavaScript Object Notation), men YAML erbjuder en mer läsbar syntax för människor.

## Se även:
- [YAML Reference](https://yaml.org/spec/1.2/spec.html)
- [Elm documentation för YAML](https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Decode-Pipeline-YAML)