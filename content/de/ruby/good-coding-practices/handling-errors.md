---
date: 2024-01-26 00:57:03.472496-07:00
description: "Wie geht das: Ruby verwendet `begin`, `rescue`, `ensure` und `end`,\
  \ um Fehler zu behandeln. Man umgibt den riskanten Code mit `begin` und `end`. Tritt\
  \ ein\u2026"
lastmod: '2024-03-13T22:44:54.407827-06:00'
model: gpt-4-1106-preview
summary: Ruby verwendet `begin`, `rescue`, `ensure` und `end`, um Fehler zu behandeln.
title: Fehlerbehandlung
weight: 16
---

## Wie geht das:
Ruby verwendet `begin`, `rescue`, `ensure` und `end`, um Fehler zu behandeln. Man umgibt den riskanten Code mit `begin` und `end`. Tritt ein Fehler auf, springt `rescue` ein.

```Ruby
begin
  # Riskanter Code kommt hier hin.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Hoppla! Das können Sie nicht tun: #{e.message}"
ensure
  puts "Das wird immer ausgeführt, mit oder ohne Fehler."
end
```

Beispielausgabe:
```
Hoppla! Das können Sie nicht tun: divided by 0
Das wird immer ausgeführt, mit oder ohne Fehler.
```

## Vertiefung
Historisch gesehen hat sich die Fehlerbehandlung in Programmiersprachen erheblich weiterentwickelt, wobei frühe Sprachen oft grobe oder nicht existente Mechanismen hatten. Rubys Ausnahmebehandlung ist von Sprachen wie Python und Smalltalk inspiriert.

Alternativen zum `begin-rescue` in Ruby schließen die Verwendung von `rescue` in Methodendefinitionen ein, oder das Verwenden von `throw` und `catch` für nicht standardisierte Ablaufsteuerungen, obwohl diese nicht für typische Fehlerbehandlungen verwendet werden.

Ein interessantes Detail: Rubys Ausnahmen sind Objekte (Instanzen der `Exception` Klasse und deren Nachkommen), so dass man benutzerdefinierte Fehlerklassen definieren und mehr als nur Fehler protokollieren kann – man kann reichhaltigen Zustand durch das Programm tragen für eine robustere Fehlerbehandlung.

## Siehe auch
- Die Ruby-Dokumentation über Ausnahmen und Fehlerbehandlung: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Ein detaillierter Leitfaden zu den besten Praktiken der Fehlerbehandlung in Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
