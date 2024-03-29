---
date: 2024-01-26 04:30:44.214195-07:00
description: "Att arbeta med XML inneb\xE4r att hantera data i ett utbrett, strukturerat\
  \ format som anv\xE4nds i konfigurationer, meddelanden och mer. Programmerare\u2026"
lastmod: '2024-03-13T22:44:38.362499-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med XML inneb\xE4r att hantera data i ett utbrett, strukturerat\
  \ format som anv\xE4nds i konfigurationer, meddelanden och mer. Programmerare\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att hantera data i ett utbrett, strukturerat format som används i konfigurationer, meddelanden och mer. Programmerare manipulerar XML för att läsa, skriva, uppdatera och fråga data – avgörande för interoperabilitet i massor av appar och tjänster.

## Hur man gör:
Fish har ingen inbyggd XML-tolkning, så du kommer att luta dig mot externa verktyg som `xmllint` eller `xmlstarlet`. Här är en kodsnutt för att läsa värden:

```fish
# Tolka XML med xmlstarlet
echo '<root><element>Hej Världen</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Utdata:
```
Hej Världen
```

För att redigera XML, använd detta:

```fish
# Redigera XML-element med xmlstarlet
echo '<root><element>Gammalt Värde</element></root>' | xmlstarlet ed -u "/root/element" -v 'Nytt Värde'
```

Utdata:
```xml
<?xml version="1.0"?>
<root>
  <element>Nytt Värde</element>
</root>
```

## Djupdykning:
XML har funnits sedan slutet av 90-talet, skapat för läsbarhet och maskinvänlighet. Medan JSON har tagit över en del av XML:s popularitet på grund av enkelheten, är XML fortfarande djupt rotat där dokumentvalidering och namnrymder är nyckeln.

Alternativ? Självklart – JSON, YAML, eller till och med binära format som Protocol Buffers för de prestandaintensiva apparna. Men XML:s schema och XSLT (för XML-transformationer) kan vara deal-breakers för komplexa scenarier där robusthet är viktigt.

Under huven använder verktyg som `xmlstarlet` kraftfulla bibliotek som libxml2, som ger dig XPath och XQuery för finjusterad XML-justering. Dessa är inte bara XML-verktyg utan portar till DOM-manipulation, eftersom du skulle tillämpa liknande koncept i vilket språk som helst som rör vid XML.

## Se också:
- [xmlstarlet Dokumentation](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish Dokumentation](https://fishshell.com/docs/current/index.html)
- [XPath och XQuery Funktioner och Operatorer](https://www.w3.org/TR/xpath-functions/)
