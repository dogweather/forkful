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

Dlaczego: 

Jeśli od dłuższego czasu zajmujesz się programowaniem w Rust, to na pewno słyszałeś o YAML. Jest to często używany format plików konfiguracyjnych, który jest czytelny dla człowieka i łatwy do parsowania. Pozwala to na przenoszenie ustawień między różnymi aplikacjami bez potrzeby pisania kodu na nowo.

## Jak to zrobić

Jeśli chcesz rozpocząć pracę z YAML, powinieneś najpierw dodać odpowiednią bibliotekę do swojego projektu. W tym przypadku użyjemy popularnego krateczki "yaml-rust". Możesz to zrobić, dodając poniższe linie do swojego pliku Cargo.toml:

```Rust
[dependencies]
yaml-rust = "0.4.4"
```

Następnie, w kodzie, musisz zaimportować bibliotekę:

```Rust
extern crate yaml_rust;
```

Aby przeczytać plik YAML, musisz utworzyć bufor ze swoim plikiem i użyć funkcji "load" z biblioteki "yaml-rust". Poniższy kod pokazuje, jak to zrobić:

```Rust
let file = std::fs::File::open("config.yml").unwrap();
let buffer = BufReader::new(file);
let parsed = yaml_rust::YamlLoader::load_from_reader(buffer).unwrap();
```

Możesz teraz uzyskać dostęp do wczytanych danych, używając składni jak przy mapach i wektorach w Rust. Na przykład, aby uzyskać dostęp do wartości z pliku YAML, można użyć kodu:

```Rust
let value = parsed[0]["key"].as_str().unwrap();
```

## Głębsze spojrzenie

Biblioteka "yaml-rust" oferuje również wiele innych funkcji, takich jak modyfikowanie plików YAML, sprawdzanie poprawności składni czy zapisywanie zmian do pliku. Możesz także wykorzystać funkcje "serialize" i "deserialize" do konwersji danych Rust na YAML i odwrotnie.

Ważne jest również pamiętać, że format YAML jest wrażliwy na wcięcia, więc należy zwrócić szczególną uwagę na prawidłową strukturę pliku. W przeciwnym razie może to spowodować błędy podczas parsowania.

Nadal są dostępne inne biblioteki obsługujące YAML w języku Rust, więc nie wahaj się poszukać innych opcji, jeśli ta nie spełnia Twoich potrzeb. 

## Zobacz także

- Dokumentacja oficjalna biblioteki "yaml-rust": https://docs.rs/yaml-rust/ 
- Przykładowy projekt wykorzystujący YAML: https://github.com/johnbhogue/rust-yaml-example 
- Poradnik wprowadzający do pracy z YAML w Rust: https://deterministic.space/yaml-rust.html