---
title:                "Praca z językiem YAML"
html_title:           "Elixir: Praca z językiem YAML"
simple_title:         "Praca z językiem YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

Czym jest YAML i dlaczego programiści go używają?

YAML (Yet Another Markup Language) jest językiem do opisywania danych w sposób czytelny dla człowieka. Jest to format pliku używany do przechowywania i przesyłania informacji w prosty i zrozumiały sposób. Programiści najczęściej używają YAML do konfiguracji i przetwarzania struktury danych.

Jak to zrobić?

Aby pracować z YAML w Elixir, możemy skorzystać z biblioteki o nazwie YamlElixir. Najpierw musimy zainstalować tę bibliotekę przy użyciu narzędzia Mix, a następnie uruchomić komendę `mix deps.get` aby pobrać wymagane zależności. Następnie, zaimportujmy bibliotekę do naszego modułu i wywołajmy funkcję `YamlElixir.load()` aby załadować dane ze struktury YAML.

 ```Elixir
defmodule MyModule do
  import YamlElixir
  data = YamlElixir.load("my_config.yml")
end
```

Ponadto, możemy wykorzystać funkcję `YamlElixir.to_yaml()` aby utworzyć plik YAML z naszej struktury danych w Elixir.

 ```Elixir
defmodule MyModule do
  import YamlElixir
  data = %{"name" => "John", "age" => 30, "job" => "developer"}
  yaml = YamlElixir.to_yaml(data)
end
```

Głębszy zanurzenie:

YAML został wprowadzony w 2001 roku przez Clarka Evansa. Jest łatwy w użyciu i zrozumiały dla ludzi, co czyni go dobrym wyborem dla konfiguracji i przetwarzania danych. Alternatywą dla YAML jest JSON, jednak YAML jest bardziej przyjazny dla użytkownika dzięki możliwości formatowania tekstu i wsparciu dla komentarzy.

See Also:

• Oficjalna dokumentacja YamlElixir: https://hexdocs.pm/yaml_elixir

• Przykładowy plik YAML: https://yaml.org/spec/1.2/spec.html

• Porównanie między YAML a JSON: https://dzone.com/articles/yaml-vs-json-its-a-difference-of-roots-and-formati