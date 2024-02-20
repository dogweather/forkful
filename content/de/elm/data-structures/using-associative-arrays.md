---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:49.601580-07:00
description: "Assoziative Arrays, oder wie sie in Elm genannt werden, Dictionaries,\
  \ ordnen Schl\xFCsseln Werte zu, was das Suchen, Einf\xFCgen und L\xF6schen von\
  \ Werten super\u2026"
lastmod: 2024-02-19 22:05:12.723097
model: gpt-4-0125-preview
summary: "Assoziative Arrays, oder wie sie in Elm genannt werden, Dictionaries, ordnen\
  \ Schl\xFCsseln Werte zu, was das Suchen, Einf\xFCgen und L\xF6schen von Werten\
  \ super\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, oder wie sie in Elm genannt werden, Dictionaries, ordnen Schlüsseln Werte zu, was das Suchen, Einfügen und Löschen von Werten super schnell macht. Sie sind die erste Wahl, wenn Sie Dinge ohne strikte Reihenfolge verfolgen müssen, wie Benutzereinstellungen oder Inventarlisten.

## Wie geht das:

In Elm arbeitet man mit Dictionaries im `Dict`-Modul, also lass uns in ein schnelles Beispiel eintauchen:

```Elm
import Dict exposing (Dict)

-- Initialisiert ein Dictionary mit String-Schlüsseln und Int-Werten
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Fügt einen Wert hinzu oder aktualisiert ihn
updatedDict = Dict.insert "grape" 10 exampleDict

-- Einen Wert abrufen (beachten Sie den Maybe-Typ, da der Schlüssel nicht vorhanden sein könnte)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Ein Schlüssel-Wert-Paar entfernen
finalDict = Dict.remove "banana" updatedDict

-- Ein Dictionary zurück in eine Liste umwandeln
dictToList = Dict.toList finalDict
```

Beispielausgabe beim Anzeigen von `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Dies demonstriert die grundlegenden Operationen: erstellen, aktualisieren, zugreifen und iterieren über ein Dictionary.

## Tiefergehend

Dictionaries in Elm verwenden intern eine Struktur, die als AVL-Baum bekannt ist - eine Art von selbstausgleichendem binären Suchbaum. Diese Wahl schafft ein Gleichgewicht zwischen der Sicherstellung, dass Operationen wie Einfügen, Abrufen und Entfernen eine gute Leistung (logarithmische Zeitkomplexität) haben und der Einfachheit in der Handhabung der Daten.

Trotz der Stärken von Elms `Dict` ist es keine Lösung für alle Fälle. Für Sammlungen, die geordnet sind oder sequenziell iteriert werden müssen, könnten List oder Array angemessener sein. Weiterhin, wenn man mit einer festgelegten Menge von bekannten Schlüsseln arbeitet, könnten benutzerdefinierte Typen (Elms Version von Enums) mehr Typsicherheit und eine klarere Absicht in Ihrem Code bieten.

Im Ökosystem von Elm bietet `Dict` eine zuverlässige Möglichkeit, Sammlungen von Schlüssel-Wert-Paaren zu verwalten, bei denen die Schlüssel einzigartig sind und die Reihenfolge keine Rolle spielt. Während neuere oder raffiniertere Strukturen auftauchen können, bleibt das `Dict`-Modul ein grundlegendes Werkzeug in der Werkzeugkiste des Elm-Programmierers für seine Einfachheit und Effizienz im Umgang mit assoziativen Arrays.
