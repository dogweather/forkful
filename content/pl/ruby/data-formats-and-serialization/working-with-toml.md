---
date: 2024-01-26 04:25:53.853171-07:00
description: "TOML to format pliku konfiguracyjnego, kt\xF3ry jest \u0142atwy do odczytu\
  \ dzi\u0119ki swojej jasnej semantyce. Programi\u015Bci u\u017Cywaj\u0105 TOML do\
  \ zarz\u0105dzania konfiguracjami\u2026"
lastmod: '2024-03-11T00:14:09.177957-06:00'
model: gpt-4-0125-preview
summary: "TOML to format pliku konfiguracyjnego, kt\xF3ry jest \u0142atwy do odczytu\
  \ dzi\u0119ki swojej jasnej semantyce. Programi\u015Bci u\u017Cywaj\u0105 TOML do\
  \ zarz\u0105dzania konfiguracjami\u2026"
title: Praca z TOML
---

{{< edit_this_page >}}

## Co i dlaczego?

TOML to format pliku konfiguracyjnego, który jest łatwy do odczytu dzięki swojej jasnej semantyce. Programiści używają TOML do zarządzania konfiguracjami aplikacji i serializacją danych bez obciążenia XML czy dziwactw YAML.

## Jak to zrobić:

Najpierw zainstaluj gem `toml-rb`. Jest to popularny wybór do parsowania TOML w Ruby.

```Ruby
gem install toml-rb
```

Następnie, odczyt pliku TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Przykładowe wyjście może być:

```
My Awesome App
```

Pisanie do pliku TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Sprawdź `config.toml`, a zobaczysz swoje ustawienia, schludnie zapisane.

## Głębsze zanurzenie

TOML, co oznacza Tom's Obvious, Minimal Language, został stworzony przez Toma Preston-Wernera, współzałożyciela GitHuba, około 2013 roku. Jego głównym celem jest bycie prostym formatem łatwym do sparsowania do struktur danych. Chociaż JSON jest świetny dla API, a YAML elastyczny, nisza TOML polega na podkreśleniu przyjazności dla ludzi. W przeciwieństwie do YAML, który może być kapryśny ze względu na wcięcia, TOML dąży do struktury bardziej przypominającej INI, którą wielu uważa za prostszą i mniej podatną na błędy.

Alternatywy takie jak JSON, YAML czy XML mają każdy swoje mocne strony, ale TOML odnosi sukcesy w scenariuszach, gdzie konfiguracja powinna być łatwa do utrzymania zarówno przez ludzi, jak i programy. Nie jest tylko prostszy, ale wymusza ścisłe i czytelne formatowanie.

Od strony technicznej, do parsowania zawartości TOML z Ruby, korzystamy z gemów takich jak `toml-rb`. Ten gem wykorzystuje dynamiczną naturę Ruby, konwertując dane TOML do natywnych hashy Ruby, tablic i innych podstawowych struktur danych. Ta konwersja oznacza, że programiści mogą pracować z danymi TOML, używając znajomej semantyki i metod Ruby.

## Zobacz także

- Projekt i specyfikacja TOML: https://toml.io/en/
- Gem `toml-rb`: https://github.com/emancu/toml-rb
- Porównanie TOML, YAML i JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
