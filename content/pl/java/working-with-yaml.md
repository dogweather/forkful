---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? | Co i Dlaczego?
YAML to format danych używany do konfiguracji aplikacji. Programiści lubią YAML, bo jest czytelny dla człowieka i elastyczny w użytkowaniu.

## How to: | Jak to zrobić:
```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        InputStream inputStream = YamlExample.class
          .getClassLoader()
          .getResourceAsStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```
Powyższy kod wczytuje plik `config.yaml` i wypisuje jego zawartość. Nie zapomnij dodać zależności SnakeYAML do swojego projektu.

## Deep Dive | Dogłębna analiza
YAML, czyli "YAML Ain't Markup Language", powstał w 2001 roku. Alternatywami dla YAML są JSON i XML, które są bardziej surowe w strukturze. YAML pozwala na reprezentowanie hierarchicznych danych w bardziej naturalny sposób i jest używany w projektach takich jak Docker czy Kubernetes. Podczas pracy z YAML stosuje się biblioteki takie jak SnakeYAML w Javie, umożliwiające parsowanie i generowanie danych w formacie YAML.

## See Also | Zobacz także
- Oficjalna strona YAML: https://yaml.org
- Dokumentacja SnakeYAML: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Tutorial YAML w Javie: https://www.baeldung.com/java-snake-yaml
