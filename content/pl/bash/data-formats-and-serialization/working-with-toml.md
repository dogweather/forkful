---
date: 2024-01-26 04:19:03.513881-07:00
description: "Jak to zrobi\u0107: Na pocz\u0105tek zainstaluj `toml-cli`, aby bawi\u0107\
  \ si\u0119 TOML w Bashu. Przydatne do czytania lub edycji plik\xF3w TOML na bie\u017C\
  \u0105co."
lastmod: '2024-03-13T22:44:35.608833-06:00'
model: gpt-4-0125-preview
summary: "Na pocz\u0105tek zainstaluj `toml-cli`, aby bawi\u0107 si\u0119 TOML w Bashu."
title: Praca z TOML
weight: 39
---

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
