---
title:                "Praca z yaml"
html_title:           "Java: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-yaml.md"
---

{{< edit_this_page >}}

# Czym jest YAML i dlaczego programiści go używają?

YAML jest to język do przetwarzania strukturalnych danych, często wykorzystywany przez programistów w projektach programistycznych. Jest to wygodne narzędzie do przechowywania i przesyłania danych w prosty i czytelny sposób.

Jednym z powodów używania YAML przez programistów jest jego prostota i czytelność. Dzięki zastosowaniu struktury w postaci "klucz-wartość", pliki YAML są łatwe do zrozumienia i edycji przez ludzi, a także przez programy komputerowe.

# Jak to zrobić:

Aby pracować z YAML w Javie, potrzebujesz biblioteki YAMLBeans, która umożliwia łatwe wczytywanie i zapisywanie plików YAML. Następnie możesz wykorzystać różne metody biblioteki, aby przetwarzać dane w plikach YAML.

Przykładowy kod:

```Java
import org.yamlbeans.YamlException;
import org.yamlbeans.YamlReader;
import org.yamlbeans.YamlWriter;

// wczytanie pliku YAML
YamlReader reader = new YamlReader(new FileReader("dane.yaml"));
Object obj = reader.read();

// zapisanie danych do pliku YAML
YamlWriter writer = new YamlWriter(new FileWriter("dane.yaml"));
writer.write(obj);
writer.close();
```

Przykładowe dane w pliku YAML:

```yaml
- autor: John Smith
  tytul: Java dla początkujących
- autor: Anna Kowalska
  tytul: Programowanie w Javie na zaawansowanym poziomie
```

# Głębsze wgląd:

YAML został stworzony przez Clarka Evansa w 2001 roku i początkowo był wykorzystywany w języku programowania Perl. Obecnie jest stosowany w wielu językach programowania, w tym w Javie.

Alternatywą dla YAML w świecie Javy jest format XML, jednak YAML jest uważany za bardziej czytelny i łatwiejszy w użyciu. Inne języki programowania wykorzystujące YAML to między innymi Python, Ruby czy PHP.

Główną zaletą YAML jest jego elastyczność - może być używany do przechowywania różnych typów danych, w tym napisów, liczb, list i słowników.

# Zobacz także:

- [Oficjalna strona YAML](https://yaml.org/)
- [Dokumentacja YAMLBeans](https://yamlbeans.sourceforge.io/)