---
date: 2024-01-26 04:19:03.513881-07:00
description: "TOML, czyli Tom's Obvious, Minimal Language, to format serializacji\
  \ danych. Programi\u015Bci ceni\u0105 go za prostot\u0119 i czytelno\u015B\u0107\
  ; jest \u015Bwietny do plik\xF3w\u2026"
lastmod: '2024-02-25T18:49:33.976484-07:00'
model: gpt-4-0125-preview
summary: "TOML, czyli Tom's Obvious, Minimal Language, to format serializacji danych.\
  \ Programi\u015Bci ceni\u0105 go za prostot\u0119 i czytelno\u015B\u0107; jest \u015B\
  wietny do plik\xF3w\u2026"
title: Praca z TOML
---

{{< edit_this_page >}}

## Co i dlaczego?
TOML, czyli Tom's Obvious, Minimal Language, to format serializacji danych. Programiści cenią go za prostotę i czytelność; jest świetny do plików konfiguracyjnych, ma podobny klimat do YAML, ale jest mniej uciążliwy niż JSON dla człowieka.

## Jak to zrobić:
Na początek zainstaluj `toml-cli`, aby bawić się TOML w Bashu. Przydatne do czytania lub edycji plików TOML na bieżąco.

```Bash
# Zainstaluj toml-cli, naszego małego pomocnika do zadań TOML
pip install toml-cli

# Wyobraź sobie, że masz plik TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Odczyt wartości
toml get config.toml owner.name
# Wyjście: Tom

# Ustaw wartość
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Wskazówka: Używaj cudzysłowu dla kluczy z kropkami lub dziwnymi znakami!
```

## Pogłębione spojrzenie
TOML powstał z niechęci do przeszkód, jakie JSON stawiał ludziom, pojawił się w okolicach 2013 roku. Tom Preston-Werner, współzałożyciel GitHuba, chciał czegoś super czytelnego. YAML i INI były alternatywami, ale TOML jest jak najlepsze z obu. 

Aż tu nagle, masz zagnieżdżone dane i tablice, minus pułapki YAML i klamry JSON. TOML teraz to pierwszy wybór dla konfiguracji w Cargo Rusta, co mówi o jego wzroście w świecie deweloperów. Jest napędzany specyfikacją, utrzymującą rzeczy sprawnie i precyzyjnie. Znajdziesz parsery w prawie każdym języku, co czyni go szeroko adoptowalnym.

## Zobacz także
- Oficjalne repozytorium TOML na GitHubie: https://github.com/toml-lang/toml
- toml-cli na PyPI: https://pypi.org/project/toml-cli/
- Porównanie formatów serializacji danych: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
