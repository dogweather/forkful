---
date: 2024-01-26 04:18:12.707352-07:00
description: "Jak to zrobi\u0107: Na ten moment Rust nie ma oficjalnej REPL do\u0142\
  \u0105czonej do siebie. Mo\u017Cna u\u017Cywa\u0107 narz\u0119dzi stron trzecich\
  \ takich jak `evcxr_repl`. Zainstaluj je\u2026"
lastmod: '2024-03-13T22:44:35.186273-06:00'
model: gpt-4-0125-preview
summary: "Na ten moment Rust nie ma oficjalnej REPL do\u0142\u0105czonej do siebie."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
Na ten moment Rust nie ma oficjalnej REPL dołączonej do siebie. Można używać narzędzi stron trzecich takich jak `evcxr_repl`. Zainstaluj je za pomocą Cargo:

```sh
cargo install evcxr_repl
```

Następnie uruchom REPL:

```sh
evcxr
```

Wewnątrz przetestuj kod Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Wynik powinien wyglądać tak:

```
5 + 3 = 8
```

## Pogłębiona analiza
Etyka Rusta koncentruje się wokół bezpieczeństwa i wydajności, które zwykle kojarzone są z językami kompilowanymi przed czasem wykonania, a mniej z językami interpretowanymi, przyjaznymi dla REPL. Historycznie, języki takie jak Python czy Ruby priorytetyzowały posiadanie REPL dla natychmiastowego feedbacku, ale nie zostały zaprojektowane z myślą o zadaniach na poziomie systemowym.

Pomimo braku oficjalnej REPL w Rust, pojawiło się kilka alternatyw takich jak `evcxr_repl`. Te projekty nie tylko wprowadzają Rust do REPL w sposób siłowy; inteligentnie łączą cykl kompilacji i uruchamiania języka w interaktywną sesję. REPL kompiluje kod w tle i uruchamia binarny, przechwytując wyjście. W ten sposób zachowuje korzyści wydajnościowe Rust, jednocześnie zapewniając tę interaktywną doświadczenie.

Trwa dyskusja w społeczności Rust na temat oficjalnego wsparcia REPL, i z każdą iteracją języka widzimy coraz większą sofistykcję narzędzi, co ostatecznie może prowadzić do natywnego rozwiązania.

## Zobacz również
Aby uzyskać więcej informacji i innych narzędzi:
- Repozytorium Evcxr REPL na GitHubie: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, sposób online na testowanie kodu Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Dyskusja na temat funkcji REPL w języku Rust: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
