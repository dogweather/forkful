---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
("## Co i dlaczego?")
YAML to format przechowywania danych łatwy do czytania dla człowieka. Programiści używają go do konfiguracji aplikacji i zarządzania danymi, zapewniając prostotę i czytelność.

## How to:
("## Jak to zrobić:")
```Bash
# Czytanie pliku YAML w Bash przy użyciu Pythona i PyYAML
cat << EOF > config.yaml
ścieżka: /dom/
użytkownicy:
  - name: jan
  - name: ewa
EOF

# Instalacja PyYAML, jeśli potrzebna
pip install pyyaml

# Czytanie i wypisywanie wartości 'ścieżka' z pliku YAML
python3 -c 'import yaml; print(yaml.safe_load(open("config.yaml"))["ścieżka"])'
# Wypisze: /dom/
```

## Deep Dive
("## Szczegóły")
YAML wywodzi się z XML i INI, oferując lepszą czytelność. Alternatywami są JSON i TOML; jednak YAML jest bardziej przyjazny dla użytkownika. Implementacja w Bash często wymaga użycia zewnętrznych narzędzi, np. Python z biblioteką PyYAML, ponieważ bash nie posiada natywnej obsługi YAML.

## See Also
("## Zobacz też")
- Oficjalna strona YAML: [https://yaml.org](https://yaml.org)
- PyYAML dokumentacja: [https://pyyaml.org/wiki/PyYAMLDocumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- YAML Lint, do walidacji YAML online: [http://www.yamllint.com/](http://www.yamllint.com/)