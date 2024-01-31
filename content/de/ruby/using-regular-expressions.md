---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke, kurz Regex, sind Muster zur Textsuche und -manipulation. Sie sind mächtig für das Parsen von Strings, Validieren von Eingaben und das Filtern von Daten.

## Wie geht das:
```Ruby
text = "Ruby ist fantastisch. Ruby-Version 3.1.2"

# Findet das Wort "Ruby"
puts text.scan(/Ruby/)

# Prüft, ob der String mit "Ruby" beginnt
puts text.start_with?("Ruby")

# Ersetzt "fantastisch" durch "großartig"
puts text.gsub(/fantastisch/, 'großartig')
```
Ausgabe:
```
Ruby
Ruby
Ruby ist großartig. Ruby-Version 3.1.2
```

## Tiefer Eintauchen:
Reguläre Ausdrücke stammen aus der theoretischen Informatik und wurden in den 1950er-Jahren populär. Alternativen zu Regex sind spezialisierte Parser oder String-Funktionen, die allerdings oft weniger flexibel sind. Bei der Implementierung in Ruby wird meist die Bibliothek Oniguruma verwendet, die vollständige Regex-Funktionalität ermöglicht.

## Siehe auch:
- Ruby-Dokumentation zu Regex: [Ruby Regexp](https://ruby-doc.org/core-3.1.2/Regexp.html)
- Online Regex Tester und Debugger: [Rubular](http://rubular.com/)
- Regex Lernressourcen: [RegexOne](https://regexone.com/)
