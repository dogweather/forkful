---
title:                "Arbeta med yaml"
html_title:           "Swift: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Arbetar du som programmerare och har hört talas om YAML men är osäker på vad det är och varför man använder det? YAML är ett enkelt sätt att strukturera data med hjälp av indentering och punktnotation. Programmörer använder det vanligtvis för konfigurationsfiler och dataöverföring mellan applikationer.

## Hur man:
Det första steget är att installera YAML-paketet i ditt Swift-projekt. Sedan kan du enkelt skriva YAML-kod inuti en ```Swift ...``` kodblock. Här är ett exempel på hur en YAML-fil kan se ut:
```
Swift func createYAML() { let myYAML = """ 
name: John Doe 
age: 25 
occupation: Programmer
""" print(myYAML) } createYAML() 
```
Och här är den resulterande utmatningen:
```
"name: John Doe
age: 25
occupation: Programmer"
```
Med hjälp av YAML-paketet kan du också enkelt konvertera YAML-format till JSON och vice versa.

## Djupdykning:
YAML (YAML Ain't Markup Language) är ett programmeringsmönster som utvecklades 2001 av Ingy döt Net och Ingy döt Törrö. Det är ett ytterligare alternativ för JSON och XML för att kunna strukturera data. YAML är enklare att läsa och skriva för människor eftersom det inte behöver några XML-tagg eller kommatecken. Det finns också YAML-implementeringar för många andra programmeringsspråk, inklusive Swift.

## Se även:
För mer information om YAML och hur du kan implementera det i ditt Swift-projekt, se följande länkar:

- [YAML.org](http://www.yaml.org/)
- [Yaml Swift GitHub Repository](https://github.com/behrang/YamlSwift)