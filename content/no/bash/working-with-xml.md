---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:27:40.198453-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML innebærer parsing, ekstrahering og manipulering av data i Extensible Markup Language-formatet. Programmere sliter med XML siden det er et utbredt datautvekslingsformat for konfigurasjoner, APIer og mer.

## Hvordan:
Her er hvordan du parser XML i Bash. Verktøy? xmllint og xmlstarlet. Løkke gjennom XML-elementer? Absolutt. Eksempel med eksempel på utdata:

```bash
# Antar at xmlstarlet er installert
# Installer med: apt-get install xmlstarlet

# Parse XML-innhold
cat <<EOF > eksempel.xml
<frukter>
  <frukt navn="Eple"/>
  <frukt navn="Banan"/>
</frukter>
EOF

# Ekstraher navn med xmlstarlet
xmlstarlet sel -t -m "//frukt" -v "@navn" -n eksempel.xml

# Utdata bør være:
# Eple
# Banan
```

##Dypdykk
Tilbake på 90-tallet dukket XML opp som et enklere alternativ til SGML, men mer strukturert enn HTML. Nå har det selskap – JSON og YAML, for eksempel. Men XML holder fortsatt koken, spesielt i konfigurasjoner og SOAP-baserte webtjenester.

Når det gjelder verktøy, er xmllint behagelig for XML-validering og xpath-spørringer. xmlstarlet er den sveitsiske armékniven for XML-kunster – spørre, redigere, validere, transformere. I bash-skript er de superhelter for XML-oppgaver.

Under hetten bruker xmllint libxml2 – XML C-parseren. Det er raskt, men feilmeldingene? Kryptiske. Og xmlstarlet? Rekursive maler og EXSLT-støtte. Tankebøy, men kraftig.

## Se Også
- [xmlsoft.org](http://xmlsoft.org/): Libxml2 og xmllint-stoff.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Problemer og løsninger fra den virkelige verden.
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/): Grunnleggende om XML.
