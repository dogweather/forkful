---
date: 2024-01-27 16:21:11.924149-07:00
description: "Wie: Ruby bietet eine unkomplizierte M\xF6glichkeit, Dateien direkt\
  \ von der Kommandozeile aus zu bearbeiten. Mit Rubys `-i` Schalter k\xF6nnen Sie\
  \ Ruby\u2026"
lastmod: '2024-03-13T22:44:54.396387-06:00'
model: gpt-4-0125-preview
summary: "Ruby bietet eine unkomplizierte M\xF6glichkeit, Dateien direkt von der Kommandozeile\
  \ aus zu bearbeiten."
title: Bearbeiten von Dateien im Place mit CLI-Einzeilern
weight: 32
---

## Wie:
Ruby bietet eine unkomplizierte Möglichkeit, Dateien direkt von der Kommandozeile aus zu bearbeiten. Mit Rubys `-i` Schalter können Sie Ruby mitteilen, direkt auf die bereitgestellten Datei(en) zu operieren. Lassen Sie uns mit einigen Beispielen spielen, um zu sehen, wie das im echten Leben funktioniert. Stellen Sie sich vor, Sie haben eine Datei `greetings.txt` mit dem folgenden Inhalt:

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

Und Sie möchten das Wort "Hello" mit "Hi" ersetzen. So können Sie das tun:

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

Nachdem Sie diesen Befehl ausgeführt haben, wird `greetings.txt` auf Folgendes aktualisiert:

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

Wenn Sie besorgt sind, Daten möglicherweise zu verpfuschen, hat Ruby eine Lösung parat. Indem Sie eine Erweiterung zum `-i` Schalter angeben, erstellt Ruby vor der Ausführung der Änderungen ein Backup. Zum Beispiel:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

Jetzt finden Sie neben Ihrer bearbeiteten `greetings.txt` eine `greetings.txt.bak` im selben Verzeichnis vor, die den ursprünglichen Inhalt enthält.

## Tiefergehend
Die Magie der In-Place-Dateibearbeitung in Ruby ergibt sich aus der Kombination von Perl-ähnlichen Textverarbeitungsmöglichkeiten und der eigenen syntaktischen Eleganz von Ruby. Historisch gesehen war Perl die bevorzugte Sprache für schnelle Einzeiler-Skripte, insbesondere für Textmanipulationen. Ruby hat dieses Paradigma übernommen und ermöglicht leistungsfähige Kommandozeilenskripting-Fähigkeiten.

Alternativen für die In-Place-Bearbeitung gibt es auch in anderen Sprachen, wie Perl selbst und sed, einem Stream-Editor in Unix-Systemen. Jede hat ihre Stärken – Perl ist bekannt für seine Textverarbeitungsmacht, während sed in seiner Einfachheit für Stream-Editing-Aufgaben unübertroffen ist. Jedoch bietet Ruby eine Balance, indem es robuste Textmanipulation mit einer lesbareren und benutzerfreundlicheren Syntax bietet, besonders für diejenigen, die bereits mit Ruby vertraut sind.

Von der Implementierungsseite her funktioniert Rubys In-Place-Bearbeitung, indem die ursprüngliche Datei umbenannt wird, eine neue mit dem ursprünglichen Dateinamen erstellt wird und dann die Änderungen in diese neue Datei geschrieben werden, während sie aus der umbenannten Originaldatei liest. Dieser Ansatz gewährleistet die Atomarität der Operation; entweder wird die gesamte Datei erfolgreich verarbeitet, oder es werden keine Änderungen vorgenommen, was die Integrität Ihrer Daten während des Bearbeitungsprozesses schützt. Dieser Mechanismus, kombiniert mit Rubys Ausnahmebehandlung, bietet auch eine Widerstandsfähigkeit gegen Unterbrechungen, wie Stromausfälle oder Prozessabbrüche, und stellt sicher, dass zumindest das Backup intakt bleibt.

Zusammenfassend ist Rubys In-Place-Dateibearbeitung ein Beweis für seine Nützlichkeit als Skriptsprache und bietet eine Mischung aus Kraft, Einfachheit und Eleganz für Textmanipulationsaufgaben direkt von der Kommandozeile.
