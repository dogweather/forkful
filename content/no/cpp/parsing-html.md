---
title:                "Analysering av html"
html_title:           "C++: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse HTML er prosessen med å analysere og forstå HTML-kode som brukes til å lage nettsider. Dette gjøres av programvare for å kunne vise nettsider korrekt på en enhet. Programvareutviklere gjør dette for å sørge for at deres nettsider fungerer som de skal, uavhengig av enheten det blir vist på.

## Hvordan:
 Slik ser enkel HTML-kode ut:
```C++
<html>
<head>
<title>Min Nettside</title>
</head>
<body>
<h1>Velkommen!</h1>
<p>Dette er min første nettside.</p>
</body>
</html>
```
 
Til å begynne med, kan vi bruke et parser-bibliotek som heter **libxml** for å lese og tolke HTML-koden. Vi må først inkludere header-filen ```<libxml/HTMLParser.h>``` for å bruke dette biblioteket. Deretter kan vi bruke funksjonen ```htmlParseDoc``` for å lese og tolke HTML-koden. Her er et eksempel på hvordan man kan hente ut tittelen fra HTML-koden:
 
```C++
#include <iostream>
#include <libxml/HTMLParser.h>

int main(){
  htmlDocPtr doc = htmlParseDoc("<html><head><title>Min Nettside\
</title></head><body></body></html>", NULL);
  xmlChar* title = xmlNodeGetContent(doc->children->children->children->next->children->children);
  std::cout << "Tittelen på nettsiden er: " << title;
  xmlFreeDoc(doc);
  return 0;
}
```

**Output:**
```
Tittelen på nettsiden er: Min Nettside
```

## Dypdykk:
Parsing av HTML har vært en viktig del av webutvikling helt siden starten av internett og HTML. Det finnes flere alternativer til **libxml**, som for eksempel **pugixml** og **TinyXML**, som også kan brukes for å lese og tolke HTML-kode. Disse alternativene kan ha forskjellige funksjoner og ytelse, så det er viktig å finne den som passer best for ditt prosjekt.

En viktig del av å parse HTML er å forstå hvordan HTML-koden er strukturert og å kunne navigere gjennom tags og elementer. Dette kan være utfordrende, spesielt for mer kompleks HTML-kode. Derfor er det viktig å bli kjent med de forskjellige parser-metodene som finnes og å øve seg på å lese og manipulere HTML-kode.

## Se også:
https://github.com/leffavp/HTML-Parser - et komplett eksempelprosjekt for å lære mer om parsing av HTML i C++.