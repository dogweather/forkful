---
title:                "Praca z YAML"
aliases:
- pl/bash/working-with-yaml.md
date:                  2024-02-03T19:24:51.040048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, czyli YAML Ain't Markup Language, to standard serializacji danych czytelnych dla człowieka, który może być używany do plików konfiguracyjnych, jak również w aplikacjach, gdzie dane są przechowywane lub przesyłane. Programiści skłaniają się ku YAML ze względu na jego przejrzystość i prostotę, szczególnie w projektach wymagających skomplikowanych konfiguracji lub potrzeby łatwo edytowalnych struktur danych.

## Jak to zrobić:

Bezpośrednia praca z YAML w Bashu wymaga nieco pomysłowości, ponieważ Bash nie ma wbudowanego wsparcia dla parsowania YAML. Jednak możesz użyć zewnętrznych narzędzi, takich jak `yq` (lekkie i przenośne narzędzie do wiersza poleceń dla YAML), aby efektywnie współdziałać z plikami YAML. Przejdźmy przez kilka typowych operacji:

### Instalacja `yq`:

Zanim zagłębimy się w przykłady, upewnij się, że masz zainstalowany `yq`. Zwykle możesz go zainstalować z menedżera pakietów, na przykład na Ubuntu:

```bash
sudo apt-get install yq
```

Lub możesz pobrać go bezpośrednio z repozytorium GitHub.

### Odczytywanie wartości:

Załóżmy, że masz plik o nazwie `config.yaml` z następującą zawartością:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

Aby odczytać hosta bazy danych, możesz użyć `yq` w następujący sposób:

```bash
yq e '.database.host' config.yaml
```

**Przykładowy wynik:**

```
localhost
```

### Aktualizowanie wartości:

Aby zaktualizować nazwę użytkownika w `config.yaml`, użyj polecenia `yq eval` z opcją `-i` (in-place):

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Zweryfikuj zmianę za pomocą:

```bash
yq e '.user.name' config.yaml
```

**Przykładowy wynik:**

```
newadmin
```

### Dodawanie nowego elementu:

Aby dodać nowy element w sekcji bazy danych, jak nowe pole `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Sprawdzenie zawartości pliku potwierdzi dodanie.

### Usuwanie elementu:

Aby usunąć hasło użytkownika:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Ta operacja usunie pole hasła z konfiguracji.

Pamiętaj, `yq` jest potężnym narzędziem i ma wiele więcej możliwości, w tym konwersję YAML na JSON, łączenie plików i nawet bardziej złożone manipulacje. Zapoznaj się z dokumentacją `yq` dla dalszych poszukiwań.
