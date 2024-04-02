---
date: 2024-01-26 04:17:07.129036-07:00
description: "Eine interaktive Shell oder REPL (Read-Eval-Print Loop) erm\xF6glicht\
  \ es, Code in Echtzeit zu testen. Programmierer nutzen sie, um zu experimentieren,\
  \ Fehler\u2026"
lastmod: '2024-03-13T22:44:54.402034-06:00'
model: gpt-4-0125-preview
summary: "Eine interaktive Shell oder REPL (Read-Eval-Print Loop) erm\xF6glicht es,\
  \ Code in Echtzeit zu testen. Programmierer nutzen sie, um zu experimentieren, Fehler\u2026"
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Was & Warum?
Eine interaktive Shell oder REPL (Read-Eval-Print Loop) ermöglicht es, Code in Echtzeit zu testen. Programmierer nutzen sie, um zu experimentieren, Fehler zu finden und die Feinheiten von Ruby zu lernen, ohne vollständige Skripte erstellen zu müssen.

## Wie geht das:
Die REPL von Ruby heißt IRB (Interactive Ruby). Spring rein und probiere Ruby direkt aus deinem Terminal:

```Ruby
irb
2.7.0 :001 > puts "Hallo, Ruby-Welt!"
Hallo, Ruby-Welt!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Tiefer Eintauchen
Eingeführt in Ruby 1.8, ist IRB ein Grundnahrungsmittel für Rubyisten. Es ist inspiriert von den interaktiven Shells von Lisp und Python, und verbindet Experimentieren mit sofortigem Feedback. Alternativen wie Pry bieten mehr Funktionen, wie Syntax-Hervorhebung und eine robustere Debugging-Umgebung. IRB selbst ist einfach, kann aber mit Gems wie 'irbtools' erweitert werden, um die Funktionalität zu erweitern. Wie IRB die Read-Eval-Print-Schleife handhabt, besteht darin, jede Zeile der Eingabe zu lesen, sie als Ruby-Code zu bewerten und dann das Ergebnis zu drucken, und diesen Prozess bis zum Beenden zu wiederholen.

## Siehe auch
- [Rubys IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Das irbtools gem](https://github.com/janlelis/irbtools)
