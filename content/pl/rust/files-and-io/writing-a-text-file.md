---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:19.767781-07:00
description: "Pisanie plik\xF3w tekstowych w Rust polega na tworzeniu, zapisywaniu\
  \ i potencjalnie dodawaniu danych do pliku w systemie plik\xF3w. Programi\u015B\
  ci wykonuj\u0105 t\u0119\u2026"
lastmod: 2024-02-19 22:04:54.330487
model: gpt-4-0125-preview
summary: "Pisanie plik\xF3w tekstowych w Rust polega na tworzeniu, zapisywaniu i potencjalnie\
  \ dodawaniu danych do pliku w systemie plik\xF3w. Programi\u015Bci wykonuj\u0105\
  \ t\u0119\u2026"
title: Pisanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie plików tekstowych w Rust polega na tworzeniu, zapisywaniu i potencjalnie dodawaniu danych do pliku w systemie plików. Programiści wykonują tę operację, aby zachować dane, takie jak logi aplikacji, konfiguracja czy treści generowane przez użytkowników, zapewniając trwałość danych poza zakresem wykonania programu.

## Jak to zrobić:
Standardowa biblioteka Rusta oferuje solidne narzędzia do manipulacji plikami, głównie zawarte w modułach `std::fs` i `std::io`. Oto podstawowy przykład, jak stworzyć i zapisać do pliku tekstowego:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Witaj, świecie!")?;
    Ok(())
}
```

Po uruchomieniu tego kodu znajdziesz plik o nazwie `hello.txt` z zawartością "Witaj, świecie!".

Dla bardziej złożonych scenariuszy, takich jak dodawanie do istniejącego pliku czy efektywne obsługiwanie większych danych, Rust oferuje dodatkowe funkcjonalności. Oto jak dodać tekst do istniejącego pliku:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Dodaję więcej tekstu.")?;
    Ok(())
}
```

Uruchomienie tego spowoduje dodanie " Dodaję więcej tekstu." na końcu `hello.txt`.

W niektórych przypadkach wykorzystanie bibliotek stron trzecich może uproszczać operacje na plikach. Na przykład zestawienie `serde` z `serde_json` pozwala na serializację i deserializację struktur danych do i z formatu JSON, oferując wysokopoziomowe podejście do zapisu plików:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

Po uruchomieniu powyższego kodu, `user.json` będzie zawierał reprezentację JSON struktury `User`. Należy zauważyć, że użycie `serde` i `serde_json` wymaga dodania tych skrzynek do pliku `Cargo.toml`.

Pisanie plików tekstowych w Rust, zarówno za pomocą biblioteki standardowej, jak i z pomocą zewnętrznych skrzynek, jest prostym, a jednocześnie potężnym sposobem na zarządzanie trwałością danych w twoich aplikacjach.
