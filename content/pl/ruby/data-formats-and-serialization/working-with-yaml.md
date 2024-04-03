---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:32.647378-07:00
description: "YAML, co oznacza YAML Ain't Markup Language (YAML to nie j\u0119zyk\
  \ znacznik\xF3w), jest intensywnie u\u017Cywany w Ruby dla plik\xF3w konfiguracyjnych\
  \ oraz serializacji\u2026"
lastmod: '2024-03-13T22:44:35.952474-06:00'
model: gpt-4-0125-preview
summary: "YAML, co oznacza YAML Ain't Markup Language (YAML to nie j\u0119zyk znacznik\xF3\
  w), jest intensywnie u\u017Cywany w Ruby dla plik\xF3w konfiguracyjnych oraz serializacji\
  \ danych z powodu swojego formatu czytelnego dla cz\u0142owieka."
title: Praca z YAML
weight: 41
---

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
