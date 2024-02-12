---
title:                "Pisanie do standardowego błędu"
aliases: - /pl/rust/writing-to-standard-error.md
date:                  2024-02-03T19:34:53.367298-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie do standardowego błędu"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) w Rust oznacza kierowanie komunikatów o błędach i diagnostyki na konsolę oddzielnie od standardowego wyjścia (stdout). Programiści robią to, aby odróżnić normalne wyjście programu od komunikatów o błędach, co ułatwia odpowiednie obsługiwane błędów lub przekierowywanie ich do dzienników lub plików podczas wykonania.

## Jak to zrobić:
Rust oferuje prosty sposób na pisanie do stderr za pomocą makra `eprintln!`, podobnie jak `println!` jest używane dla stdout. Oto podstawowy przykład:

```rust
fn main() {
    eprintln!("To jest komunikat o błędzie!");
}
```

Przykładowe wyjście (do standardowego błędu):
```
To jest komunikat o błędzie!
```

Aby mieć większą kontrolę nad komunikatami o błędach, na przykład kiedy chcesz formatować tekst lub obsługiwać wyniki operacji I/O, użyj funkcji `stderr` z modułu `std::io`. Ta metoda dostarcza uchwyt do globalnego strumienia stderr, do którego możesz następnie pisać, używając metod takich jak `write_all` lub `writeln` z cechy `Write`:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Sformatowany komunikat o błędzie: {}", 404).expect("Nie udało się zapisać do stderr");
}
```

Przykładowe wyjście (do standardowego błędu):
```
Sformatowany komunikat o błędzie: 404
```

Jeśli pracujesz w środowiskach lub aplikacjach, gdzie polegasz na bibliotekach do logowania lub obsługi błędów, popularne są takie biblioteki jak `log` i `env_logger`. Chociaż są one używane bardziej do celów logowania, są konfigurowalne i mogą kierować poziomy logowania błędów do stderr. Poniżej znajduje się prosty przykład użycia `log` i `env_logger`:

Najpierw dodaj zależności do pliku `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Następnie, skonfiguruj i użyj logowania w swojej aplikacji:
```rust
fn main() {
    env_logger::init();
    log::error!("Ten komunikat o błędzie został zalogowany do stderr");
}
```

Uruchomienie tego programu (po skonfigurowaniu `env_logger` z odpowiednią zmienną środowiskową, na przykład `RUST_LOG=error`) spowoduje wyjście komunikatu o błędzie do stderr, wykorzystując infrastrukturę logowania.

```plaintext
ERROR: Ten komunikat o błędzie został zalogowany do stderr
```
