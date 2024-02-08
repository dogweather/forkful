---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- de/rust/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:35.680358-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
In der Softwareentwicklung ist es oft notwendig zu überprüfen, ob ein Verzeichnis existiert, um Fehler zu vermeiden, wenn versucht wird, Dateien zu öffnen, zu lesen oder zu schreiben. Rust, als eine Systemprogrammiersprache, bietet robuste Methoden, um diese Aufgabe auszuführen und sicherzustellen, dass Ihr Programm Dateien und Verzeichnisse sicher und effizient handhaben kann.

## Wie:
Die Standardbibliothek von Rust (`std`) enthält Funktionen, um die Existenz eines Verzeichnisses über die Module `std::path::Path` und `std::fs` zu überprüfen. Hier ist ein einfaches Beispiel, das Rusts Standardansatz verwendet:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("Das Verzeichnis existiert.");
    } else {
        println!("Das Verzeichnis existiert nicht.");
    }
}
```

Beispielausgabe, unter der Annahme, dass das Verzeichnis existiert:
```
Das Verzeichnis existiert.
```

Für komplexere Szenarien oder erweiterte Funktionen (wie asynchrone Dateisystemoperationen) könnten Sie in Erwägung ziehen, eine Drittanbieterbibliothek wie `tokio` mit ihrem asynchronen `fs` Modul zu verwenden, besonders wenn Sie in einer asynchronen Laufzeitumgebung arbeiten. So könnten Sie das Gleiche mit `tokio` erreichen:

Fügen Sie zunächst `tokio` zu Ihrem `Cargo.toml` hinzu:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

Verwenden Sie dann `tokio::fs`, um asynchron zu überprüfen, ob ein Verzeichnis existiert:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Das Verzeichnis existiert.");
            } else {
                println!("Der Pfad existiert, ist aber kein Verzeichnis.");
            }
        },
        Err(_) => println!("Das Verzeichnis existiert nicht."),
    }
}
```

Beispielausgabe, unter der Annahme, dass das Verzeichnis nicht existiert:
```
Das Verzeichnis existiert nicht.
```

Diese Beispiele heben hervor, wie Rust und sein Ökosystem sowohl synchrone als auch asynchrone Ansätze zur Überprüfung der Existenz von Verzeichnissen bieten und damit eine breite Palette von Softwareentwicklungsbedürfnissen abdecken.
