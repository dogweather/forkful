---
date: 2024-01-26 04:30:50.510130-07:00
description: "Lavorare con XML significa gestire dati in un formato strutturato e\
  \ pervasivo, usato in configurazioni, messaggistica e altro. I programmatori manipolano\u2026"
lastmod: '2024-03-13T22:44:43.885246-06:00'
model: gpt-4-0125-preview
summary: Lavorare con XML significa gestire dati in un formato strutturato e pervasivo,
  usato in configurazioni, messaggistica e altro.
title: Lavorare con XML
weight: 40
---

## Cosa e Perché?
Lavorare con XML significa gestire dati in un formato strutturato e pervasivo, usato in configurazioni, messaggistica e altro. I programmatori manipolano XML per leggere, scrivere, aggiornare e interrogare dati—fondamentali per l'interoperabilità in molte app e servizi.

## Come fare:
Fish non ha un parsing XML integrato, quindi ti affiderai a strumenti esterni come `xmllint` o `xmlstarlet`. Ecco uno snippet per leggere i valori:

```fish
# Analizzare XML usando xmlstarlet
echo '<root><element>Ciao Mondo</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Output:
```
Ciao Mondo
```

Per modificare XML, usa questo:

```fish
# Modificare l'elemento XML usando xmlstarlet
echo '<root><element>Vecchio Valore</element></root>' | xmlstarlet ed -u "/root/element" -v 'Nuovo Valore'
```

Output:
```xml
<?xml version="1.0"?>
<root>
  <element>Nuovo Valore</element>
</root>
```

## Approfondimento:
XML è presente dagli anni '90, creato per essere leggibile e amichevole per le macchine. Mentre JSON ha usurpato parte della popolarità di XML grazie alla sua semplicità, XML rimane radicato dove la validazione dei documenti e i namespace sono chiave.

Alternative? Certo—JSON, YAML, o anche formati binari come Protocol Buffers per quelle app ad alte prestazioni. Ma lo schema XML e XSLT (per le trasformazioni XML) possono essere determinanti in scenari complessi dove la robustezza conta.

Sotto il cofano, strumenti come `xmlstarlet` avvolgono potenti librerie come libxml2, offrendoti XPath e XQuery per un tinkering XML dettagliato. Questi non sono solo strumenti XML ma gateway per la manipolazione del DOM, come applicheresti concetti simili in qualsiasi linguaggio che tocca XML.

## Vedi anche:
- [Documentazione di xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Documentazione di Fish](https://fishshell.com/docs/current/index.html)
- [Funzioni e Operatori XPath e XQuery](https://www.w3.org/TR/xpath-functions/)
