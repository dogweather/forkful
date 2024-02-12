---
title:                "Praca z YAML"
aliases:
- /pl/go/working-with-yaml/
date:                  2024-02-03T18:13:47.911420-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z YAML w Go polega na parsowaniu plików YAML (YAML Ain't Markup Language), czyli ludzko-przyjaznego standardu serializacji danych, na struktury danych w Go i odwrotnie. Programiści robią to, aby wykorzystać prostotę i czytelność YAML dla plików konfiguracyjnych, ustawień aplikacji lub wymiany danych między usługami i komponentami napisanymi w różnych językach.

## Jak to zrobić:

Aby pracować z YAML w Go, najpierw musisz zaimportować bibliotekę obsługującą parsowanie i serializację YAML, ponieważ standardowa biblioteka Go nie obejmuje bezpośredniego wsparcia dla YAML. Najpopularniejszą biblioteką do tego celu jest "gopkg.in/yaml.v3". Oto jak zacząć:

1. **Instalacja pakietu YAML:**

```bash
go get gopkg.in/yaml.v3
```

2. **Parsowanie YAML do struktury Go:**

Najpierw zdefiniuj strukturę w Go, która pasuje do struktury Twoich danych YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("Użytkownik: %s\nHasło: %s\n", config.Database.User, config.Database.Password)
}
```

**Przykładowy wynik:**

```
Użytkownik: admin
Hasło: secret
```

3. **Serializacja struktury Go do YAML:**

Oto, jak przekształcić strukturę Go z powrotem na YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      Użytkownik: "admin",
      Hasło:      "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Przykładowy wynik:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## W głąb:

Zastosowanie YAML w rozwoju oprogramowania wzrosło ze względu na jego format przyjazny dla człowieka, czyniąc go idealnym wyborem dla plików konfiguracyjnych, dokumentacji czy formatów wymiany danych. W porównaniu z JSON, jego odpowiednikiem, YAML oferuje komentarze, typy skalarnie i funkcje relacji, dostarczając bardziej bogaty framework serializacji danych. Jednak jego elastyczność i funkcje mają swoją cenę w postaci złożoności parsowania, co prowadzi do potencjalnych ryzyk bezpieczeństwa, gdy nie jest obsługiwany z należytą ostrożnością (np. wykonanie dowolnego kodu).

Biblioteka "gopkg.in/yaml.v3" dla Go jest solidnym rozwiązaniem do przetwarzania YAML, znajdując równowagę między łatwością użycia a wszechstronnym wsparciem funkcji. Stan obecny, chociaż istnieją alternatywy takie jak "go-yaml/yaml" (biblioteka stojąca za "gopkg.in/yaml.v3"), wybór konkretnej wersji zwykle zależy od konkretnych wymagań projektu lub osobistych preferencji. W przypadku pracy z ogromnymi zestawami danych lub aplikacjami krytycznymi pod względem wydajności, programiści mogą rozważyć prostsze formaty, takie jak JSON, ze względu na ich krótszy czas parsowania i mniejsze zużycie pamięci. Niemniej jednak, dla plików konfiguracyjnych lub ustawień, w których kluczowe są czytelność i łatwość użycia, YAML pozostaje silnym konkurentem w ekosystemie Go.
