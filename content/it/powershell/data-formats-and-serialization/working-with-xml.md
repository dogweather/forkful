---
date: 2024-01-26 04:34:12.387290-07:00
description: "Lavorare con XML implica manipolare e accedere a dati strutturati nel\
  \ linguaggio di markup estensibile (eXtensible Markup Language). I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.666471-06:00'
model: gpt-4-0125-preview
summary: Lavorare con XML implica manipolare e accedere a dati strutturati nel linguaggio
  di markup estensibile (eXtensible Markup Language).
title: Lavorare con XML
weight: 40
---

## Cosa & Perché?
Lavorare con XML implica manipolare e accedere a dati strutturati nel linguaggio di markup estensibile (eXtensible Markup Language). I programmatori lavorano con XML per abilitare l'interoperabilità con altri sistemi o per leggere e scrivere file di configurazione, flussi di dati e altri documenti strutturati comuni nei servizi web.

## Come fare:
```PowerShell
# Caricare un file XML in una variabile
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# Accedere ai nodi XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Titolo: $($book.title)"
}

# Creare un nuovo elemento XML
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Salvare l'XML modificato su file
$xmlContent.Save('path\to\your\updated\file.xml')
```
Esempio di Output:
```
Titolo: Programmazione PowerShell
Titolo: Fondamenti di XML
```

## Approfondimento
XML, o linguaggio di markup estensibile, esiste dagli anni '90 ed è ancora un formato ampiamente utilizzato per dati strutturati. PowerShell semplifica il lavoro con XML rispetto ai metodi di parsing tradizionali; converte direttamente l'XML in oggetti, permettendoti di interagire con gli elementi attraverso una notazione a punti familiare.

Le alternative a XML includono JSON, YAML o formati di dati personalizzati. JSON, ad esempio, ha guadagnato popolarità per la sua leggerezza e facilità d'uso con le tecnologie web. Tuttavia, le funzionalità estese di XML come gli spazi dei nomi, gli schemi e l'elaborazione XSLT spesso lo rendono più adatto per documenti complessi o standard di settore.

PowerShell utilizza le capacità XML del .NET Framework per la sua gestione di XML. Questo significa che non si tratta solo di semplici operazioni di lettura-scrittura; puoi anche lavorare con schemi XML per la validazione, usare XPath per le query e impiegare trasformazioni XSLT, tutto tramite PowerShell.

## Vedi Anche
- [Tutorial XML di W3Schools](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
