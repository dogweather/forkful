---
title:                "Praca z YAML"
date:                  2024-02-03T19:26:32.647378-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
YAML, co oznacza YAML Ain't Markup Language (YAML to nie język znaczników), jest intensywnie używany w Ruby dla plików konfiguracyjnych oraz serializacji danych z powodu swojego formatu czytelnego dla człowieka. Programiści skłaniają się ku YAML, gdy potrzebują przechowywać lub przesyłać obiekty danych w sposób czytelny, lecz strukturalny, co upraszcza zadania takie jak zarządzanie konfiguracją, przechowywanie danych i udostępnianie danych między językami.

## Jak to zrobić:
Ruby posiada wbudowaną bibliotekę o nazwie Psych do parsowania i emitowania YAML. Aby jej użyć, najpierw musisz zażądać standardowej biblioteki YAML. Oto podstawowy przykład, który pozwoli Ci zacząć:

```ruby
require 'yaml'

# Hash do serializacji
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Konwersja hasha na YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Przykładowe Wyjście:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Aby wczytać dane YAML z powrotem do obiektu Ruby:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Przykładowe Wyjście:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Używanie Bibliotek Stron Trzecich:

Chociaż standardowa biblioteka wystarcza do podstawowych zadań, dla bardziej skomplikowanych potrzeb możesz zainteresować się gemami stron trzecich, takimi jak 'safe_yaml'. Aby użyć takich bibliotek, musisz najpierw zainstalować gema:

```bash
gem install safe_yaml
```

Następnie, możesz go używać do bezpiecznego wczytywania danych YAML, minimalizując ryzyka, takie jak instancjonowanie obiektów z kontrolowanych przez użytkownika źródeł:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Przykładowe Wyjście:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

To podejście zwiększa bezpieczeństwo obsługi YAML, czyniąc je dobrą opcją dla aplikacji wczytujących YAML z niezaufanych źródeł.
