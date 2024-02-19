---
aliases:
- /de/ruby/working-with-toml/
date: 2024-01-26 04:25:38.570581-07:00
description: "TOML ist ein Konfigurationsdateiformat, das aufgrund seiner klaren Semantik\
  \ leicht zu lesen ist. Programmierer nutzen TOML, um App-Konfigurationen und\u2026"
lastmod: 2024-02-18 23:09:05.448244
model: gpt-4-0125-preview
summary: "TOML ist ein Konfigurationsdateiformat, das aufgrund seiner klaren Semantik\
  \ leicht zu lesen ist. Programmierer nutzen TOML, um App-Konfigurationen und\u2026"
title: Arbeiten mit TOML
---

{{< edit_this_page >}}

## Was & Warum?

TOML ist ein Konfigurationsdateiformat, das aufgrund seiner klaren Semantik leicht zu lesen ist. Programmierer nutzen TOML, um App-Konfigurationen und Daten-Serialisierung zu verwalten, ohne die Schwere von XML oder die Eigenheiten von YAML.

## Wie geht das:

Zuerst installieren Sie das `toml-rb` Gem. Es ist eine beliebte Wahl für das Parsen von TOML in Ruby.

```Ruby
gem install toml-rb
```

Als Nächstes lesen Sie eine TOML-Datei:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Ein Beispieloutput könnte sein:

```
Meine fantastische App
```

In eine TOML-Datei schreiben:

```Ruby
require 'toml-rb'

config = {
  'title' => 'Meine fantastische App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Überprüfen Sie `config.toml` und Sie werden Ihre Einstellungen, ordentlich gespeichert, sehen.

## Tiefer Eintauchen

TOML, das für Toms Offensichtliche, Minimale Sprache steht, wurde etwa 2013 von Tom Preston-Werner, dem Mitbegründer von GitHub, erstellt. Sein Hauptziel ist es, ein unkompliziertes Format zu sein, das leicht in Datenstrukturen zu parsen ist. Während JSON großartig für APIs ist und YAML flexibel ist, liegt die Nische von TOML in seinem Schwerpunkt auf Benutzerfreundlichkeit. Im Gegensatz zu YAML, das mit Einrückungen wählerisch sein kann, strebt TOML nach einer mehr INI-ähnlichen Struktur, die viele einfacher und weniger fehleranfällig finden.

Alternativen wie JSON, YAML oder XML haben jeweils ihre eigenen Stärken, aber TOML blüht in Szenarien auf, in denen eine Konfiguration leicht von Menschen und Programmen gleichermaßen gewartet werden sollte. Es ist nicht nur einfacher, sondern erzwingt auch eine strenge und lesbare Formatierung.

Auf der technischen Seite nutzen wir, um TOML-Inhalte mit Ruby zu parsen, Gems wie `toml-rb`. Dieses Gem nutzt die dynamische Natur von Ruby und konvertiert TOML-Daten in native Ruby-Hashes, Arrays und andere grundlegende Datenstrukturen. Diese Konvertierung bedeutet, dass Entwickler mit TOML-Daten mit den vertrauten Ruby-Semantiken und Methoden arbeiten können.

## Siehe auch

- TOML-Projekt und Spezifikation: https://toml.io/en/
- Das `toml-rb` Gem: https://github.com/emancu/toml-rb
- Vergleich von TOML, YAML und JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
