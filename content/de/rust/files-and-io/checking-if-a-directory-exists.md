---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:35.680358-07:00
description: "Wie: Die Standardbibliothek von Rust (`std`) enth\xE4lt Funktionen,\
  \ um die Existenz eines Verzeichnisses \xFCber die Module `std::path::Path` und\
  \ `std::fs` zu\u2026"
lastmod: '2024-03-13T22:44:53.687360-06:00'
model: gpt-4-0125-preview
summary: "Die Standardbibliothek von Rust (`std`) enth\xE4lt Funktionen, um die Existenz\
  \ eines Verzeichnisses \xFCber die Module `std::path::Path` und `std::fs` zu \xFC\
  berpr\xFCfen."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

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
