---
date: 2024-01-26 04:27:35.065879-07:00
description: "Come fare: Ecco come analizzare XML in Bash. Strumenti? xmllint e xmlstarlet.\
  \ Iterare attraverso gli elementi XML? Decisamente. Esempio con output di\u2026"
lastmod: '2024-03-13T22:44:43.623867-06:00'
model: gpt-4-0125-preview
summary: Ecco come analizzare XML in Bash.
title: Lavorare con XML
weight: 40
---

## Come fare:
Ecco come analizzare XML in Bash. Strumenti? xmllint e xmlstarlet. Iterare attraverso gli elementi XML? Decisamente. Esempio con output di esempio:

```bash
# Assumendo che xmlstarlet sia installato
# Installa con: apt-get install xmlstarlet

# Analisi del contenuto XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Estrai i nomi con xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# L'output dovrebbe essere:
# Apple
# Banana
```

## Approfondimento
Negli anni '90, XML è emerso come un'alternativa più semplice a SGML, ma più strutturata di HTML. Ora ha compagnia - JSON, YAML, ad esempio. Ma XML è ancora in gioco, specialmente nelle configurazioni e nei servizi web basati su SOAP.

Per quanto riguarda gli strumenti, xmllint è comodo per la validazione di XML e le query xpath. xmlstarlet è il coltellino svizzero per le marachelle XML - interrogare, modificare, validare, trasformare. Negli script bash, sono supereroi per i compiti XML.

Sotto il cofano, xmllint utilizza libxml2 - il parser C di XML. È veloce, ma i messaggi di errore? Criptici. E xmlstarlet? Template ricorsivi e il supporto EXSLT. Difficile da capire, ma potente.

## Vedi Anche
- [xmlsoft.org](http://xmlsoft.org/): Roba di Libxml2 e xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Problemi e soluzioni del mondo reale.
- [Tutorial XML di W3Schools](https://www.w3schools.com/xml/): Basi di XML.
