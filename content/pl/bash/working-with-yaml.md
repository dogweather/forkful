---
title:                "Praca z yaml"
html_title:           "Bash: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Praca z YAML jest często nieuniknionym wyzwaniem dla programistów. Jest to język formatowania, który został stworzony do przechowywania i przesyłania danych w sposób czytelny dla człowieka. Programiści wykorzystują go w swojej pracy, aby zorganizować i przechowywać informacje w sposób łatwy do przeczytania i zrozumienia.

## Jak to zrobić:

By pracować z formatem YAML w Bashu, musisz zainstalować narzędzie o nazwie "yq" za pomocą menedżera pakietów. Następnie możesz używać komendy "yq" w swoim skrypcie Bash, aby czytać lub zapisywać dane YAML.

```Bash
# Przykładowe polecenie do odczytu danych YAML:
yq r <nazwa_pliku.yaml> <ścieżka_do_elementu>

# Przykładowe polecenie do zapisu danych YAML:
yq w -i <nazwa_pliku.yaml> <ścieżka_do_elementu> <wartość>
```

Wykorzystanie tego narzędzia pozwala na łatwą manipulację danymi YAML w skryptach Bash. Przykładowe wyjście z komendy odczytu może wyglądać tak:

```Bash
# Przykładowe wyjście z odczytu danych YAML:
test:
  - name: John
    age: 25
  - name: Anna
    age: 30
```

## Głębszy wywód:

YAML (Yet Another Markup Language) został stworzony w 2001 roku jako alternatywa dla języka XML. Jego prosty i czytelny format szybko zyskał popularność wśród programistów, szczególnie tych, którzy pracują z konfiguracjami lub przekazują duże ilości danych.

Alternatywą dla YAML jest m.in. język JSON (JavaScript Object Notation), który jest popularny w aplikacjach internetowych. Jednakże, JSON jest bardziej ograniczony w swojej składni i nie jest tak czytelny jak YAML.

Implementacja komendy "yq" jest oparta o bibliotekę "jq", która jest wykorzystywana do manipulacji danych w formacie JSON. Jest to ważne dla programistów, ponieważ pozwalają one na łatwe przetwarzanie i konwertowanie danych pomiędzy formatami YAML i JSON.

## Zobacz również:

- Oficjalna dokumentacja narzędzia "yq": https://github.com/mikefarah/yq
- Porównanie formatów YAML i JSON: https://yaml.org/start.html
- Poradnik o składni YAML: https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html