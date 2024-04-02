---
date: 2024-01-26 03:38:47.927202-07:00
description: "Das Entfernen von Anf\xFChrungszeichen aus einem String bedeutet, diese\
  \ l\xE4stigen einfachen (' ') oder doppelten (\" \") Anf\xFChrungszeichen von Ihren\
  \ Textdaten\u2026"
lastmod: '2024-03-13T22:44:54.296116-06:00'
model: gpt-4-0125-preview
summary: "Das Entfernen von Anf\xFChrungszeichen aus einem String bedeutet, diese\
  \ l\xE4stigen einfachen (' ') oder doppelten (\" \") Anf\xFChrungszeichen von Ihren\
  \ Textdaten\u2026"
title: "Anf\xFChrungszeichen aus einem String entfernen"
weight: 9
---

## Was & Warum?

Das Entfernen von Anführungszeichen aus einem String bedeutet, diese lästigen einfachen (' ') oder doppelten (" ") Anführungszeichen von Ihren Textdaten zu entfernen. Programmierer tun dies oft, um Eingaben zu bereinigen oder Daten zur weiteren Verarbeitung ohne das Durcheinander von Anführungszeichen vorzubereiten.

## Wie geht das:

Fish hat eine eingebaute Magie für diese Art von Aufgaben. Verwende die `string` Funktion, ohne ins Schwitzen zu kommen. Sieh dir diese Zaubertricks an:

```fish
# Beispiel mit einfachen Anführungszeichen
set quoted "'Hallo, Welt!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Ausgabe: Hallo, Welt!

# Dasselbe mit doppelten Anführungszeichen
set double_quoted "\"Hallo, Universum!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Ausgabe: Hallo, Universum!
```

## Tief eintauchen

Zurück in der Steinzeit der Kommandozeile würdest du dich mit `sed` oder `awk` herumschlagen, um Anführungszeichen zu entfernen; ein echtes Durcheinander aus Backslashes und kryptischen Flags. Fish's `string` Funktion stammt aus einer neueren Ära und macht den Code sauberer und intuitiver.

Alternativen in anderen Shells könnten immer noch auf diese alten Werkzeuge angewiesen sein oder könnten ihre eigenen eingebauten Methoden wie Bashs Parametererweiterung oder zsh's Modifikatoren verwenden.

Die `string` Funktion geht über das Trimmen von Anführungszeichen hinaus. Es ist ein Schweizer Taschenmesser für String-Operationen in Fish. Mit `string` kannst du Strings in deinem Terminal schneiden, teilen, zusammenfügen oder sogar mit Regex-Mustern abgleichen.

## Siehe auch

Tauche tiefer in `string` ein mit Hilfe der offiziellen Dokumentation:
- [Fish Shell String Dokumentation](https://fishshell.com/docs/current/commands.html#string)

Für Nostalgie oder beim Scripten mit traditionelleren Shells, sieh dir an:
- [Sed & Awk Anleitung](https://www.grymoire.com/Unix/Sed.html)
- [Bash Parametererweiterung](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
