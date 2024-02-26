---
date: 2024-01-26 04:14:19.636100-07:00
description: "REPL, oder Read-Eval-Print Loop (Lese-Auswerte-Drucke Schleife), ist\
  \ eine interaktive Programmierumgebung, die einzelne Benutzereingaben entgegennimmt,\u2026"
lastmod: '2024-02-25T18:49:51.361204-07:00'
model: gpt-4-0125-preview
summary: "REPL, oder Read-Eval-Print Loop (Lese-Auswerte-Drucke Schleife), ist eine\
  \ interaktive Programmierumgebung, die einzelne Benutzereingaben entgegennimmt,\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
REPL, oder Read-Eval-Print Loop (Lese-Auswerte-Drucke Schleife), ist eine interaktive Programmierumgebung, die einzelne Benutzereingaben entgegennimmt, ausführt und das Ergebnis zurückgibt. Programmierer nutzen sie für sofortiges Feedback, Fehlersuche und schnelles Experimentieren mit Programmierkonzepten, ohne das Overhead des Kompilierens und Ausführens eines vollständigen Programms.

## Wie:
In Fish ist die interaktive Shell der Standardmodus beim Start. So sieht das in Aktion aus:

```Fish Shell
> set color blau
> echo "Der Himmel ist $color"
Der Himmel ist blau
```

Sie können auch eingebaute Funktionen ausführen und mit Befehlssubstitutionen spielen:

```Fish Shell
> function cheer
      echo "Los geht's Fish $argv!"
  end
> cheer Coder
Los geht's Fish Coder!
```

Nicht nur das Definieren von Funktionen, Sie können auch Code Snippets spontan ausführen und das Ergebnis sofort sehen:

```Fish Shell
> math "40 / 2"
20
```

## Tiefere Einblicke
Das Konzept von REPLs reicht zurück bis zur Programmiersprache Lisp in den 1960ern. Diese Form der interaktiven Programmierung setzte den Maßstab für Umgebungen wie Python's `ipython` und Ruby's `irb`. Fish setzt den Trend mit einem Fokus auf Benutzerfreundlichkeit und interaktive Nutzung fort.

Fish unterscheidet sich von anderen Shells wie Bash dadurch, dass es von Anfang an mit Blick auf Interaktivität konzipiert wurde. Es bietet Syntax-Hervorhebung, Auto-Vorschläge und Tab-Vervollständigungen, die es mächtig in einem REPL-Stil Workflow zu nutzen machen. Noch besser, Ihre Befehle werden erinnert und sind durchsuchbar, was wiederholtes Testen zum Kinderspiel macht.

Alternativen zu Fish's REPL könnten `bash` oder `zsh` sein, wenn sie mit Erweiterungen wie `bash-completion` oder `oh-my-zsh` gepaart sind, aber Fish bietet tendenziell ein reicheres Erlebnis out-of-the-box.

## Siehe auch:
- Fish-Dokumentation: https://fishshell.com/docs/current/index.html
- Ein interessanter Vergleich von Fish gegenüber anderen Shells: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Ein tieferer Einblick in REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Interaktive Programmierung in Lisp, ein historischer Rückblick: http://www.paulgraham.com/ilisp.html
