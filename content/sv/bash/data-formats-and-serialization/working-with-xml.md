---
date: 2024-01-26 04:27:40.803297-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r hur man tolkar XML i Bash. Verktyg? xmllint\
  \ och xmlstarlet. Loopa igenom XML-element? Absolut. Exempel med utdataprovet."
lastmod: '2024-03-13T22:44:38.106509-06:00'
model: gpt-4-0125-preview
summary: "H\xE4r \xE4r hur man tolkar XML i Bash."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Här är hur man tolkar XML i Bash. Verktyg? xmllint och xmlstarlet. Loopa igenom XML-element? Absolut. Exempel med utdataprovet:

```bash
# Förutsatt att xmlstarlet är installerat
# Installera med: apt-get install xmlstarlet

# Tolka XML-innehåll
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Extrahera namn med xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Utdata bör vara:
# Apple
# Banana
```

## Djupdykning
Tillbaka på 90-talet dök XML upp som ett enklare alternativ till SGML, men mer strukturerat än HTML. Nu har det sällskap – JSON, YAML, till exempel. Men XML håller sig kvar, speciellt i konfigurationer och SOAP-baserade webbtjänster.

När det kommer till verktyg är xmllint bekvämt för XML-validering, xpath-frågor. xmlstarlet är den schweiziska armékniven för XML-upptåg – frågor, redigering, validering, omvandling. I bash-skript är de superhjältar för XML-uppgifter.

Under huven använder xmllint libxml2 – XML C-parsern. Den är snabb, men felmeddelandena? Kryptiska. Och xmlstarlet? Rekursiva mallar och stöd för EXSLT. Hjärnbrytande, men kraftfullt.

## Se även
- [xmlsoft.org](http://xmlsoft.org/): Libxml2 och xmllint-grejer.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Verkliga problem och lösningar.
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/): Grundläggande om XML.
