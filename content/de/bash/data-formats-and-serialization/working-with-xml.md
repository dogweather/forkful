---
date: 2024-01-26 04:27:25.199448-07:00
description: "Die Arbeit mit XML umfasst das Parsen, Extrahieren und Manipulieren\
  \ von Daten im Extensible Markup Language-Format. Programmierer ringen mit XML,\
  \ da es\u2026"
lastmod: '2024-03-11T00:14:27.983672-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit XML umfasst das Parsen, Extrahieren und Manipulieren von\
  \ Daten im Extensible Markup Language-Format. Programmierer ringen mit XML, da es\u2026"
title: Arbeiten mit XML
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
