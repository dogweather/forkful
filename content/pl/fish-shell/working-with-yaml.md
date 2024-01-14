---
title:                "Fish Shell: Praca z językiem YAML"
simple_title:         "Praca z językiem YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Z programowaniem w języku Fish Shell spotykamy się głównie w kontekście pracy z plikami konfiguracyjnymi. Jednym z popularnych formatów do przechowywania takich ustawień jest YAML. Pozwala on na czytelne i przejrzyste zapisywanie danych, co jest niezwykle ważne dla programistów. W tym artykule dowiecie się, jak za pomocą Fish Shella możecie łatwo i efektywnie pracować z plikami YAML.

## Jak to zrobić

Fish Shell jest niezwykle przydatnym narzędziem do pracy z YAML. Aby go wykorzystać, musimy zainstalować odpowiednie rozszerzenie - [jyaml](https://github.com/joejulian/fish-jyaml). Można to zrobić za pomocą menedżera paczek, np. [Oh My Fish](https://oh-my.fish/). Po zainstalowaniu rozszerzenia, możemy już łatwo manipulować plikami YAML za pomocą poleceń w terminalu.

Pierwszym krokiem jest wczytanie pliku YAML. Przykładowy plik może wyglądać następująco:

```
pizza:
  - margherita
  - pepperoni
  - capricciosa
  - hawaiian
```

Aby wczytać te dane w Fish Shellu, użyjemy polecenia `yaml_load`:

```Fish Shell
set -g pizza (yaml_load pizza.yaml | string split "\n")
```

Powyższe polecenie wczytuje plik YAML `pizza.yaml` i dzieli go na linie, zapisując je do zmiennej `pizza`. W ten sposób, możemy wykorzystać te dane w dalszej części kodu.

Kolejnym przydatnym poleceniem jest `yaml_get`, które pozwala na pobieranie wartości z pliku YAML. Przykładowo, aby wyświetlić wszystkie rodzaje pizzy z naszego pliku, możemy użyć polecenia:

```Fish Shell
for slice in $pizza
  echo (yaml_get pizza.yaml $slice)
end
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```
margherita
pepperoni
capricciosa
hawaiian
```

To tylko kilka przykładów, jak łatwo i szybko można pracować z plikami YAML przy użyciu Fish Shella. Zachęcamy do eksperymentowania z różnymi funkcjami i dostosowywania ich do swoich potrzeb.

## Zagłębienie się w temat

Pliki YAML mogą zawierać także bardziej złożone struktury danych, takie jak listy czy słowniki. W takich przypadkach warto skorzystać z bardziej zaawansowanych funkcji Fish Shella, takich jak `yaml_keys` czy `yaml_sort`. Szczegółowe informacje na temat pracy z plikami YAML w Fish Shella możecie znaleźć w [dokumentacji jyaml](https://github.com/joejulian/fish-jyaml#function-reference).

## Zobacz także

- [Dokumentacja Fish Shella](https://fishshell.com/docs/current/index.html)
- [Oficjalna strona jyaml](https://github.com/joejulian/fish-jyaml)
- [Oh My Fish - menedżer paczek dla Fish Shella](https://oh-my.fish/)