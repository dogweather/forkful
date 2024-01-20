---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Rozpoczynanie nowego projektu to proces, gdzie programiści tworzą nowy plik lub zestaw plików z kodem programu. Robimy to, aby zbudować nowy produkt lub doświadczenie.

## Jak To Zrobić?

W Rust, nowy projekt tworzymy korzystając z Cargo, wbudowanego systemu zarządzania pakietami. Poniżej znajdziesz kroki i przykładowy kod.

```Rust
cargo new mój_projekt
cd mój_projekt
```

Po uruchomieniu tych poleceń, Cargo utworzy nowy katalog z nazwą `mój_projekt` i kilka plików w środku.

Twój nowy projekt Rust wygląda mniej więcej tak:

```Rust
mój_projekt
├── Cargo.toml
└── src
    └── main.rs
```

W `main.rs` znajdziesz prosty "Hello, world!" program:

```Rust
fn main() {
    println!("Hello, world!");
}
```

Uruchom swoją aplikację za pomocą Cargo:

```Rust
cargo run
```

Twoja aplikacja powinna wydrukować "Hello, world!".

## Pogłębione Spostrzeżenia 

Historia: Rust został stworzony przez Mozilla Research w 2010 roku z myślą o bezpieczeństwie, prędkości i równoległości.

Alternatywy: Możesz rozpocząć projekt w Rust inaczej, używając innych narzędzi niż Cargo, takich jak `rustc` bezpośrednio, ale Cargo jest zalecane dla większości przypadków.

Szczegóły implementacji: Gdy tworzysz nowy projekt rust, `cargo new` tworzy dla ciebie szkielet aplikacji. Plik `Cargo.toml` jest konfiguracją twojego projektu, a `src/main.rs` to główny plik źródłowy.

## Zobacz Również

- [Dokumentacja Rust] (https://www.rust-lang.org/pl/learn)
- [Książka o Rust] (https://doc.rust-lang.org/book/)
- [Podręcznik Cargo] (https://doc.rust-lang.org/cargo/)