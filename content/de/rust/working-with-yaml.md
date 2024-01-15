---
title:                "Arbeiten mit yaml"
html_title:           "Rust: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal mit der Konfiguration von Software oder dem Lesen von Log-Dateien zu tun hattest, bist du vermutlich auch auf YAML gestoßen. YAML, eine menschenlesbare Datenformatierungssprache, ist in der Welt der Softwareentwicklung weit verbreitet. Es ist eine großartige Möglichkeit, Daten in einem einfachen und intuitiven Format zu speichern.

## Wie man mit YAML in Rust arbeitet

Um mit YAML in Rust zu arbeiten, benötigst du die YAML-Crate. Hier ist ein einfaches Beispiel, wie man diese in ein Rust-Projekt einbindet:

```Rust
// Importiere die YAML-Crate
extern crate yaml_rust;
// Verwende die "Loader"-Funktion, um YAML-Daten aus einer Datei zu lesen
let data = yaml_rust::YamlLoader::load_from_str("
  name: Max
  age: 25
").expect("Fehler beim Lesen der Datei");
// Greife auf die Daten zu, indem du den Schlüssel angibst
let name = &data[0]["name"];
let age = &data[0]["age"];
// Gib die Daten aus
println!("Name: {}", name);
println!("Alter: {}", age);
```

Dieses Beispiel demonstriert einige grundlegende Konzepte der Arbeit mit YAML. Zunächst wird die YAML-Crate importiert, damit wir sie im Code verwenden können. Dann verwenden wir die Funktion "Loader", um die YAML-Daten aus einer Datei zu lesen und in eine Variable zu speichern. Anschließend können wir auf die Daten zugreifen, indem wir einfach den Schlüssel angeben. In diesem Fall greifen wir auf den Namen und das Alter zu und geben sie dann mit Hilfe von "println!" aus.

## Tiefergehende Einblicke

Das Arbeiten mit YAML in Rust kann noch weiter verfeinert werden. Zum Beispiel können wir die Daten mit Hilfe von Structs und Enums in einem benutzerfreundlicheren Format speichern. Hier ist ein Beispiel, wie das aussehen könnte:

```Rust
// Definiere ein "User"-Struct mit den gewünschten Feldern
struct User {
  name: String,
  age: u8,
}
// Implementiere die "From" Trait, um eine Konvertierung von der YAML-Crate zu ermöglichen
impl From<Yaml> for User {
  fn from(yaml: Yaml) -> Self {
    // Greife auf die Daten zu und erstelle ein neues User-Objekt
    let name = yaml["name"].as_str().expect("Fehlender Name");
    let age = yaml["age"].as_i64().unwrap() as u8;
    User { name: name.to_string(), age: age }
  }
}
// Verwende die "Loader"-Funktion, um die YAML-Daten zu lesen
let data = yaml_rust::YamlLoader::load_from_str("
  name: Max
  age: 25
").expect("Fehler beim Lesen der Datei");
// Konvertiere die Daten in ein "User"-Objekt
let user: User = data[0].into();
// Gib die Daten aus
println!("Name: {}", user.name);
println!("Alter: {}", user.age);
```

In diesem Beispiel nutzen wir die "From"-Trait, um eine benutzerdefinierte Konvertierung von der YAML-Crate zu ermöglichen. Dadurch können wir die YAML-Daten direkt in ein benutzerdefiniertes Struct konvertieren und so die Daten besser verarbeiten. Dies ist nur ein Beispiel und die Möglichkeiten sind vielfältig.

## Siehe auch

- YAML-Crate-Dokumentation: https://docs.rs/yaml-rust/latest/
- Offizielle Rust-Dokumentation: https://www.rust-lang.org/de/
- YAML-Syntax-Übersicht: https://yaml.org/spec/1.2/spec.html