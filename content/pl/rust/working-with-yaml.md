---
title:                "Rust: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z YAML może być bardzo przydatna dla programistów Rusta w wielu różnych przypadkach. Ten lekki i intuicyjny język znaczników jest szczególnie przydatny w konfiguracji aplikacji oraz przesyłaniu danych z różnych źródeł. W tym artykule dowiecie się, jak skorzystać z właściwości YAML w swoich projektach w języku Rust.

## Jak to zrobić

Rust posiada wiele bibliotek i narzędzi umożliwiających pracę z YAML. Jednym z najpopularniejszych jest Crates.io, który zawiera zestaw pakietów YAML. Aby zacząć, musimy dodać bibliotekę YAML do pliku Cargo.toml:

```Rust
[dependencies]
yaml-rust = "0.4.4"
```

Następnie możemy zaimportować bibliotekę do naszego kodu:

```Rust
extern crate yaml_rust;
```

Teraz możemy przystąpić do parsowania pliku YAML i odczytywania jego danych. W poniższym przykładzie używamy metody `load_from_str` do wczytania danych bezpośrednio z ciągu znaków:

```Rust
use yaml_rust::{YamlLoader, Yaml};
let data = "name: John
            age: 30
            hobbies:
                - coding
                - reading
           ";
let docs = YamlLoader::load_from_str(data).unwrap();
let doc = &docs[0];

let name = doc["name"].as_str().unwrap();
assert_eq!("John", name);

let age = doc["age"].as_i64().unwrap();
let hobbies = doc["hobbies"].as_vec().unwrap();
```

Tak samo jak w przykładzie powyżej, możemy odczytywać dane z pliku YAML poprzez metodę `load_from_file`:

```Rust
let doc = YamlLoader::load_from_file("config.yml").unwrap();
let name = doc["name"].as_str().unwrap();
```

## Głębszy zanurzenie

Jeśli chcemy lepiej poznać funkcjonalności YAML w Rust, możemy zanurzyć się w bardziej zaawansowane metody, takie jak tworzenie map i list oraz manipulowanie nimi. Możemy również nauczyć się obsługiwać błędy podczas parsowania plików YAML.

Możliwości są olbrzymie, dlatego warto eksperymentować i korzystać z różnych narzędzi oraz dokumentacji dostępnych dla języka Rust.

## Zobacz także

Jeśli chcesz poznać więcej o pracy z YAML w języku Rust, zapoznaj się z poniższymi linkami:

- [Dokumentacja YAML-Rust](https://docs.rs/yaml-rust/0.4.4/yaml_rust/)
- [Przykładowe projekty wykorzystujące YAML w Rust](https://github.com/search?q=yaml+language%3Arust&type=Repositories)
- [Porównanie bibliotek YAML dla języka Rust](https://www.areweideyet.com/sarkar1990/yaml-rs/yaml-rs/)

Dziękujemy za przeczytanie naszego wpisu na temat pracy z YAML w języku Rust. Mamy nadzieję, że pomoże to wam w waszych przyszłych projektach. Do zobaczenia!