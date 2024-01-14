---
title:                "Java: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego YAML jest ważnym elementem w świecie programowania? Często bywa używanie jako format konfiguracyjny w aplikacjach pisanych w języku Java. Pozwala on na przechowywanie danych w czytelnej i łatwej do modyfikacji składni, co ułatwia utrzymanie i rozwój projektów.

## Jak to zrobić

Jeśli chcesz zacząć pracę z YAML w języku Java, pierwszym krokiem jest dodanie odpowiedniego modułu do swojego projektu. Następnie możesz używać gotowych klas i metod do odczytu i zapisu danych w formacie YAML.

Przykładowy kod:

```Java
// Importowanie potrzebnych paczek
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

// Tworzenie obiektu Yaml
Yaml yaml = new Yaml();

// Odczyt pliku YAML
InputStream inputStream = this.getClass()
    .getResourceAsStream("/config.yml");
Map<String, String> config = yaml.load(inputStream);
System.out.println(config);

// Zapis danych do pliku YAML
Map<String, String> exampleMap = new HashMap<>();
exampleMap.put("key1", "value1");
exampleMap.put("key2", "value2");
yaml.dump(exampleMap, new FileWriter("example.yml"));
```

Przykładowy plik YAML:

```yaml
# Przykładowy plik konfiguracyjny
application:
  name: Moja aplikacja
  version: 1.0
database:
  host: localhost
  port: 3306
  username: root
  password: secret
```

Przykładowy output:

```yaml
application:
  name: Moja aplikacja
  version: 1.0
database:
  host: localhost
  port: 3306
  username: root
  password: secret
```

## Głębszy zanurzenie

YAML jest formatem opartym na plikach tekstowych, co oznacza, że ​​jest prosty do zrozumienia i edycji przez człowieka. Jest również łatwy do parsowania przez maszynę, co czyni go idealnym wyborem jako format konfiguracyjny w projektach pisanych w języku Java.

Jedną z zalet YAML jest możliwość definiowania własnych typów danych. Można więc łatwo przechowywać obiekty złożone, takie jak listy czy mapy, w jednym pliku YAML.

## Zobacz również

- Dokumentacja pakietu SnakeYAML: https://bitbucket.org/asomov/snakeyaml/src/default/
- Przykłady użycia YAML w języku Java: https://www.baeldung.com/java-snake-yaml
- Porównanie YAML z innymi formatami konfiguracyjnymi: https://www.baeldung.com/configuration-file-types