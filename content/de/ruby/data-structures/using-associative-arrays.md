---
aliases:
- /de/ruby/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:37.486889-07:00
description: "Assoziative Arrays, bekannter als Hashes in Ruby, erm\xF6glichen die\
  \ Zuordnung eindeutiger Schl\xFCssel zu Werten. Sie sind unverzichtbar, wenn es\
  \ darum geht,\u2026"
lastmod: 2024-02-18 23:09:05.413503
model: gpt-4-0125-preview
summary: "Assoziative Arrays, bekannter als Hashes in Ruby, erm\xF6glichen die Zuordnung\
  \ eindeutiger Schl\xFCssel zu Werten. Sie sind unverzichtbar, wenn es darum geht,\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, bekannter als Hashes in Ruby, ermöglichen die Zuordnung eindeutiger Schlüssel zu Werten. Sie sind unverzichtbar, wenn es darum geht, Elemente über einen bestimmten Verweis zu verfolgen, wie z.B. die Eigenschaften eines Objekts zu speichern oder schnell auf Daten über einen eindeutigen Identifikator zuzugreifen.

## Wie geht das:

Das Erstellen und Verwenden von Hashes in Ruby ist unkompliziert. Sie können einen leeren Hash initialisieren, ihn mit Schlüssel-Werte-Paaren füllen, auf Werte über ihre Schlüssel zugreifen und mehr. So machen Sie das:

```Ruby
# Erstellen eines Hashes
my_hash = { "name" => "John Doe", "age" => 30 }

# Eine andere Möglichkeit, einen Hash zu erstellen
another_hash = Hash.new
another_hash["position"] = "Entwickler"

# Zugriff auf Hash-Werte
puts my_hash["name"] # Ausgabe: John Doe

# Hinzufügen eines neuen Schlüssel-Werte-Paares
my_hash["language"] = "Ruby"
puts my_hash # Ausgabe: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Durch einen Hash iterieren
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Ausgabe:
# name: John Doe
# age: 30
# language: Ruby
```

Sie können auch Symbole als effizientere Schlüssel verwenden:

```Ruby
# Symbole als Schlüssel verwenden
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Ausgabe: Jane Doe
```

## Tiefere Einblicke:

Das Konzept der assoziativen Arrays ist nicht einzigartig für Ruby; viele Sprachen implementieren sie unter verschiedenen Namen, wie Dictionaries in Python oder Objekte in JavaScript (wenn sie als Schlüssel-Werte-Paare verwendet werden). In den frühen Stadien von Ruby waren Hashes etwas langsamer und nicht so vielseitig. Mit der Zeit wurde die Implementierung von Hashes in Ruby jedoch hochgradig optimiert, besonders für Symbol-Schlüssel, was sie extrem effizient für häufige Zugriffe und Aktualisierungen macht.

Ruby-Hashes zeichnen sich durch ihre syntaktische Benutzerfreundlichkeit und Flexibilität aus - Sie können fast jeden Objekttyp als Schlüssel verwenden, obwohl Symbole und Zeichenketten am häufigsten sind. Intern werden Ruby-Hashes mit einem Hashing-Algorithmus implementiert, der Geschwindigkeit und Speichereffizienz ausbalanciert, auch wenn die Anzahl der Elemente zunimmt.

Obwohl Hashes unglaublich vielseitig sind, sind sie nicht die ultimative Lösung für die Datenspeicherung in Ruby. Für geordnete Sammlungen sind Arrays geeigneter und für Mengen einzigartiger Elemente könnte ein Set eine bessere Wahl sein. Zusätzlich, für sehr komplexe Datenstrukturen, könnte das Erstellen eigener Klassen ratsam sein.

Denken Sie daran, die Wahl, ob Sie einen Hash oder andere Datenstrukturen verwenden, hängt weitgehend vom spezifischen Anwendungsfall ab – Hashes sind hervorragend für schnelle Suchvorgänge und die Aufrechterhaltung von Zuordnungen zwischen einzigartigen Schlüsseln und ihren Werten geeignet.
