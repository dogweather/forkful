---
date: 2024-01-20 17:41:27.687220-07:00
description: "How to: (Jak to zrobi\u0107:) Rust u\u017Cywa `tempfile` crate do \u0142\
  atwego tworzenia tymczasowych plik\xF3w. Poni\u017Cej znajdziesz przyk\u0142ad."
lastmod: '2024-04-05T21:53:36.640657-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Rust u\u017Cywa `tempfile` crate do \u0142atwego tworzenia\
  \ tymczasowych plik\xF3w."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to: (Jak to zrobić:)
Rust używa `tempfile` crate do łatwego tworzenia tymczasowych plików. Poniżej znajdziesz przykład:

```Rust
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    
    // Zapisz coś do pliku.
    writeln!(temp_file, "Witaj, tymczasowy świecie!")?;
    
    // Spójrzmy co tam mamy.
    let mut buf = String::new();
    temp_file.as_file_mut().read_to_string(&mut buf)?;
    println!("Temp file contains: {}", buf);
    
    Ok(())
}
```

Wyjście programu:
```
Temp file contains: Witaj, tymczasowy świecie!
```

## Deep Dive (Głębsze zanurzenie)
Tworzenie plików tymczasowych jest starym konceptem, podobnymi mechanizmami korzysta UNIX od lat 70-tych. W Rust, `tempfile` crate oferuje bezpieczne i wygodne metody zarządzania tymczasowymi plikami. Alternatywą jest używanie standardowej biblioteki `std::fs` i samodzielne zarządzanie nazwami i usunięciem plików, jednak można napotkać problemy z bezpieczeństwem dostępu i kolizją nazw.

Głównym atutem `tempfile` jest to, że robi za nas sprzątanie — tymczasowe pliki są usuwane, gdy `NamedTempFile` jest zrzucany. Crate używa systemowego API do tworzenia plików, co zapewnia bezpieczeństwo i minimalizuje ryzyko kolizji nazw.

## See Also (Zobacz również)
- [Dokumentacja `tempfile` crate](https://docs.rs/tempfile)
- [Dokumentacja `std::fs`](https://doc.rust-lang.org/stable/std/fs/)
