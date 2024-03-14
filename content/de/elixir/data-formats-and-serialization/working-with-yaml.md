---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.802756-07:00
description: "YAML, eine Abk\xFCrzung f\xFCr \"YAML Ain't Markup Language\", ist ein\
  \ f\xFCr Menschen lesbarer Standard zur Serialisierung von Daten, der h\xE4ufig\
  \ f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.556296-06:00'
model: gpt-4-0125-preview
summary: "YAML, eine Abk\xFCrzung f\xFCr \"YAML Ain't Markup Language\", ist ein f\xFC\
  r Menschen lesbarer Standard zur Serialisierung von Daten, der h\xE4ufig f\xFCr\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML, eine Abkürzung für "YAML Ain't Markup Language", ist ein für Menschen lesbarer Standard zur Serialisierung von Daten, der häufig für Konfigurationsdateien und den Datenaustausch zwischen Sprachen mit unterschiedlichen Datenstrukturen verwendet wird. Programmierer nutzen es wegen seiner Einfachheit und seiner Fähigkeit, komplexe hierarchische Daten einfach darzustellen.

## Wie dazu:

Elixir bietet keine eingebaute Unterstützung für YAML. Sie können jedoch Drittanbieter-Bibliotheken wie `yamerl` oder `yaml_elixir` verwenden, um mit YAML zu arbeiten. Hier konzentrieren wir uns auf `yaml_elixir` wegen seiner Benutzerfreundlichkeit und umfassenden Funktionen.

Fügen Sie zunächst `yaml_elixir` zu Ihren mix.exs-Abhängigkeiten hinzu:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Führen Sie dann `mix deps.get` aus, um die neue Abhängigkeit abzurufen.

### YAML lesen

Gegeben sei eine einfache YAML-Datei, `config.yaml`, die so aussieht:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

Sie können diese YAML-Datei lesen und in eine Elixir-Map umwandeln wie folgt:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Beispielverwendung
Config.read()
# Ausgabe: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### YAML schreiben

Um eine Map zurück in eine YAML-Datei zu schreiben:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# Beispielverwendung
ConfigWriter.write()
# Dies wird `new_config.yaml` mit dem angegebenen Inhalt erstellen oder überschreiben
```

Beachten Sie, wie `yaml_elixir` eine unkomplizierte Übersetzung zwischen YAML-Dateien und Elixir-Datenstrukturen ermöglicht, was es zu einer ausgezeichneten Wahl für Elixir-Programmierer macht, die mit YAML-Daten arbeiten müssen.
