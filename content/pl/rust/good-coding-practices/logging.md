---
date: 2024-01-26 01:09:19.899500-07:00
description: "Logowanie jest jak prowadzenie dziennika dla twojej aplikacji; to praktyka\
  \ rejestrowania zdarze\u0144, b\u0142\u0119d\xF3w i innych istotnych danych podczas\
  \ dzia\u0142ania\u2026"
lastmod: '2024-03-11T00:14:08.366243-06:00'
model: gpt-4-1106-preview
summary: "Logowanie jest jak prowadzenie dziennika dla twojej aplikacji; to praktyka\
  \ rejestrowania zdarze\u0144, b\u0142\u0119d\xF3w i innych istotnych danych podczas\
  \ dzia\u0142ania\u2026"
title: "Rejestrowanie zdarze\u0144"
---

{{< edit_this_page >}}

## Co i dlaczego?

Logowanie jest jak prowadzenie dziennika dla twojej aplikacji; to praktyka rejestrowania zdarzeń, błędów i innych istotnych danych podczas działania programu. Deweloperzy używają logów do diagnozowania problemów, monitorowania zachowania systemu i zbierania wglądów, które napędzają ulepszenia — to chleb powszedni wywiadu operacyjnego.

## Jak to zrobić:

Załóżmy podstawowe logowanie w Rust przy użyciu skrzynki (crate) `log`, która dostarcza fasadę logowania, oraz `env_logger`, implementację logowania dla skrzynki `log`. Najpierw dodaj je do twojego Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Teraz, skonfiguruj i zainicjuj logger w twoim `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("To jest wiadomość informacyjna.");
    warn!("To jest wiadomość ostrzegawcza.");
}
```

Uruchom swoją aplikację z `RUST_LOG=info cargo run`, a zobaczysz wyjście:

```
INFO: To jest wiadomość informacyjna.
WARN: To jest wiadomość ostrzegawcza.
```

Pobaw się zmienną środowiskową `RUST_LOG`, ustawiając ją na `error`, `warn`, `info`, `debug`, lub `trace`, aby kontrolować werbalność twoich logów.

## Dogłębna analiza

Koncepcja logowania nie jest niczym nowym; istnieje od wczesnych lat informatyki. Zanim logowanie stało się powszechne w oprogramowaniu, deweloperzy polegali na prymitywnych metodach, takich jak instrukcje wydruku lub narzędzia debuggerów, aby śledzić wykonanie programu. Wraz ze wzrostem złożoności programów rosła również potrzeba strukturalnych podejść do logowania.

W Rust, skrzynka `log` abstrahuje szczegóły implementacji logowania, pozwalając deweloperom na podłączenie różnych zaplecz logowania. Chociaż `env_logger` jest często wybierany, istnieją alternatywy, takie jak `fern`, `slog` czy `tracing`, każda z własnym zestawem funkcji i opcji konfiguracji.

Niektóre kwestie do rozważenia przy implementacji logowania to:

1. **Poziomy Logów**: Kontrola werbalności jest kluczowa. Skrzynka `log` Rust definiuje kilka poziomów logowania: error, warn, info, debug i trace, w malejącej kolejności powagi.

2. **Wydajność**: Logowanie może wpłynąć na wydajność. Jest krytyczne, by używać go oszczędnie, upewniając się, że unika się logowania na ścieżkach krytycznych dla wydajności lub nadmiernie rozwlekłych logów w produkcji.

3. **Strukturalne Logowanie**: Nowoczesne najlepsze praktyki obejmują strukturalne logowanie, w którym logi są zapisywane w formacie możliwym do odczytu przez maszyny, takim jak JSON. Biblioteki takie jak `slog` pozwalają na strukturalne logowanie w Rust, które może być indeksowane i przeszukiwane przy użyciu systemów zarządzania logami, takich jak ELK Stack lub Splunk.

4. **Asynchroniczne Logowanie**: Aby zminimalizować wpływ na główną aplikację, logowanie może być wykonane asynchronicznie. Często osiąga się to, mając bibliotekę logującą piszącą do kolejki w pamięci, a oddzielny wątek przetwarza kolejkę i zapisuje logi do miejsca docelowego.

5. **Konfiguracja**: Wiele frameworków logowania wspiera konfigurację przez zmienne środowiskowe, pliki konfiguracyjne i/lub kod. Ta elastyczność jest kluczem do dostosowywania wyjścia w różnych środowiskach (rozwój, staging, produkcja).

## Zobacz także

- Dokumentacja skrzynki `log`: https://docs.rs/log/
- Dokumentacja skrzynki `env_logger`: https://docs.rs/env_logger/
- Strona logowania Rust by Example: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- Skrzynka `slog`, alternatywny framework logowania: https://github.com/slog-rs/slog
- Tracing, framework do instrumentacji programów Rust: https://crates.io/crates/tracing
