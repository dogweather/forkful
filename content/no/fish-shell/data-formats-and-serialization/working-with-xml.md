---
date: 2024-01-26 04:30:32.955696-07:00
description: "Hvordan: Fish har ikke innebygd XML-tolk, s\xE5 du vil stole p\xE5 eksterne\
  \ verkt\xF8y som `xmllint` eller `xmlstarlet`. Her er et kodestykke for \xE5 lese\
  \ verdier."
lastmod: '2024-03-13T22:44:41.251730-06:00'
model: gpt-4-0125-preview
summary: "Fish har ikke innebygd XML-tolk, s\xE5 du vil stole p\xE5 eksterne verkt\xF8\
  y som `xmllint` eller `xmlstarlet`."
title: "\xC5 jobbe med XML"
weight: 40
---

## Hvordan:
Fish har ikke innebygd XML-tolk, så du vil stole på eksterne verktøy som `xmllint` eller `xmlstarlet`. Her er et kodestykke for å lese verdier:

```fish
# Parse XML ved hjelp av xmlstarlet
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Utdata:
```
Hello World
```

For å redigere XML, bruk dette:

```fish
# Rediger XML-element ved hjelp av xmlstarlet
echo '<root><element>Gammel Verdi</element></root>' | xmlstarlet ed -u "/root/element" -v 'Ny Verdi'
```

Utdata:
```xml
<?xml version="1.0"?>
<root>
  <element>Ny Verdi</element>
</root>
```

## Dypdykk:
XML har vært rundt siden slutten av 90-tallet, laget for lesbarhet og maskinvennlighet. Selv om JSON har tatt over noe av XMLs popularitet på grunn av enkelheten, forblir XML forankret der dokumentvalidering og navneområder er nøkkelen.

Alternativer? Sikkert—JSON, YAML, eller til og med binære formater som Protocol Buffers for de ytelsesintensive appene. Men XMLs skjema og XSLT (for XML-transformasjoner) kan være deal-breakers for komplekse scenarioer hvor robusthet betyr noe.

Under panseret pakker verktøy som `xmlstarlet` kraftige biblioteker som libxml2, og gir deg XPath og XQuery for finjustert XML-tinkering. Disse er ikke bare XML-verktøy, men portaler til DOM-manipulering, da du ville brukt lignende konsepter i ethvert språk som berører XML.

## Se også:
- [xmlstarlet Dokumentasjon](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish Dokumentasjon](https://fishshell.com/docs/current/index.html)
- [XPath og XQuery Funksjoner og Operatorer](https://www.w3.org/TR/xpath-functions/)
