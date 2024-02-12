---
title:                "Arbeiten mit XML"
aliases:
- /de/bash/working-with-xml/
date:                  2024-01-26T04:27:25.199448-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML umfasst das Parsen, Extrahieren und Manipulieren von Daten im Extensible Markup Language-Format. Programmierer ringen mit XML, da es ein weit verbreitetes Datenaustauschformat für Konfigurationen, APIs und mehr ist.

## Wie:
So parst du XML in Bash. Werkzeuge? xmllint und xmlstarlet. Durch XML-Elemente loopen? Auf jeden Fall. Beispiel mit beispielhafter Ausgabe:

```bash
# Vorausgesetzt xmlstarlet ist installiert
# Installation mit: apt-get install xmlstarlet

# XML-Inhalte parsen
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apfel"/>
  <fruit name="Banane"/>
</fruits>
EOF

# Namen mit xmlstarlet extrahieren
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Die Ausgabe sollte sein:
# Apfel
# Banane
```

## Tiefergehend
Zurück in den 90ern tauchte XML als eine einfachere Alternative zu SGML auf, aber strukturierter als HTML. Jetzt hat es Gesellschaft – JSON, YAML, zum Beispiel. Aber XML ist immer noch am Leben, besonders in Konfigurationen und SOAP-basierten Webdiensten.

Was die Werkzeuge angeht, ist xmllint bequem für XML-Validierung, xpath-Abfragen. xmlstarlet ist das Schweizer Taschenmesser für XML-Schabernack – Abfragen, Bearbeiten, Validieren, Transformieren. In Bash-Skripten sind sie Superhelden für XML-Aufgaben.

Unter der Haube verwendet xmllint libxml2 – den XML C-Parser. Er ist schnell, aber die Fehlermeldungen? Kryptisch. Und xmlstarlet? Rekursive Vorlagen und die EXSLT-Unterstützung. Kopfverdreher, aber mächtig.

## Siehe auch
- [xmlsoft.org](http://xmlsoft.org/): Sachen zu Libxml2 und xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Probleme und Lösungen aus der echten Welt.
- [W3Schools XML-Tutorial](https://www.w3schools.com/xml/): Grundlagen von XML.
