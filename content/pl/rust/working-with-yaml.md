---
title:                "Praca z formatem yaml"
html_title:           "Rust: Praca z formatem yaml"
simple_title:         "Praca z formatem yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

Co & Dlaczego?
YAML jest formatem przechowywania danych, często stosowanym w programowaniu do konfigurowania aplikacji lub przechowywania pewnych informacji w plikach. Programiści używają YAML, ponieważ jest prosty w użyciu i czytelny zarówno dla ludzi, jak i dla maszyn.

Jak to zrobić?
```Rust
// Instaluj bibliotekę `serde_yaml`
let data = "
name: John
age: 30
occupation: Programmer
";

// Wyświetl zawartość pliku YAML
let result: serde_yaml::Value = serde_yaml::from_str(data).unwrap();
println!("{:?}", result);
```
Output:
```
{Value::String("John"), Value::String("30"), Value::String("Programmer")}
```

Zanurzanie się głębiej
Początki YAML sięgają lat 90., kiedy został zaprojektowany jako alternatywa dla XML. Obecnie popularność tego formatu wynika także z jego wszechstronności i obsługi wielu języków programowania. Istnieją również inne formaty przechowywania danych, takie jak JSON czy TOML, ale YAML jest często preferowany ze względu na swoją czytelność.
Implementacja parsera YAML w Rust odbywa się za pomocą bibliotek `serde_yaml` i `syntex_syntax`, które wykorzystują parser języka Rust do przetwarzania plików YAML.
## Zobacz więcej
- [Dokumentacja biblioteki `serde_yaml`](https://docs.serde.rs/serde_yaml/)
- [Oficjalna strona YAML](https://yaml.org/)