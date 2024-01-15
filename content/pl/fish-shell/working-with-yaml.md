---
title:                "Praca z yaml"
html_title:           "Fish Shell: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub administrator systemu, prawdopodobnie już spotkałeś się z formatem plików YAML. Ta strukturalna składnia jest szeroko używana w aplikacjach internetowych i narzędziach konfiguracyjnych. W artykule poznasz, jak wykorzystać Fish Shell do pracy z plikami YAML.

## Jak to zrobić

Fish Shell oferuje wiele przydatnych funkcji, które ułatwiają pracę z plikami YAML. Przede wszystkim musisz pobrać i zainstalować rozszerzenie `yaml`, które dodaje obsługę YAML do Fish Shell. Możesz to zrobić za pomocą menedżera pakietów, np. Homebrew, jak również za pomocą narzędzia `fisher`. Po zainstalowaniu rozszerzenia, możesz zacząć pracować z plikami YAML.

```
Fish Shell> fisher install jorgebucaran/yaml
```

Aby otworzyć plik YAML w Fish Shell, wystarczy użyć polecenia `open` i podać nazwę pliku:

```
Fish Shell> open config.yml
```

Po otwarciu pliku możesz przeglądać go i edytować w Fish Shell za pomocą polecenia `yaml`:

```
Fish Shell> yaml
```

Możesz również wykorzystać polecenie `cat` do przeglądania zawartości pliku YAML w konsoli:

```
Fish Shell> cat config.yml
```

Jeśli chcesz zmienić wartość klucza w pliku YAML, możesz to zrobić za pomocą polecenia `set`:

```
Fish Shell> set config.port 8080
```

Pamiętaj, aby zachować strukturę pliku YAML, dodaj spację równoważną dla każdego poziomu zagnieżdżenia.

## Czas na głębsze zanurzenie

Fish Shell oferuje wiele funkcji do pracy z plikami YAML, w tym również możliwość przekształcania formatu YAML na inne formaty, np. JSON. Możesz to zrobić za pomocą polecenia `yaml2json` lub `json2yaml`:

```
Fish Shell> yaml2json config.yml
```

Fish Shell również obsługuje składnię YAML zwanej "anchors", która pozwala na ponowne wykorzystanie części pliku YAML w różnych miejscach. Możesz oznaczyć część pliku za pomocą znaku `&` i odwołać się do niej w innej części pliku za pomocą znaku `*`:

```yaml
server: &server
  host: localhost
  port: 3000

db:
  << : *server
  name: database
```

## Zobacz także

- [Dokumentacja Fish Shell - YAML](https://fishshell.com/docs/current/cmds/yaml.html)
- [Rozszerzenie yaml dla Fish Shell](https://github.com/jorgebucaran/yaml)
- [Poradnik pracy z YAML w Fish Shell](https://medium.com/@jorgebucaran/working-with-yaml-in-fish-shell-3de1c5b3e36c)