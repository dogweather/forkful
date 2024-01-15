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

## Dlaczego

Jeśli jesteś programistą w Javie, na pewno spotkałeś się z formatem YAML w swojej pracy lub projektach. YAML jest prostym i przejrzystym formatem danych, który stał się popularnym wyborem do przechowywania i udostępniania konfiguracji aplikacji. W tym artykule dowiesz się, dlaczego warto używać YAML w swoim kodzie Javy.

## Jak to zrobić

### Tworzenie pliku YAML

Najprostszym sposobem na stworzenie pliku YAML jest użycie klasy `Yaml` z biblioteki SnakeYAML. Poniższy przykład kodu demonstruje jak stworzyć proste drzewo danych i zapisać je do pliku YAML. 

```java
import org.yaml.snakeyaml.Yaml;
import java.io.PrintWriter;

public class CreateYamlExample {

    public static void main(String[] args) throws FileNotFoundException {
        // tworzenie drzewa danych
        HashMap<String, Object> dataTree = new HashMap<>();
        dataTree.put("name", "John");
        dataTree.put("age", 30);
        dataTree.put("occupation", "developer");

        // inicjalizacja obiektu YAML
        Yaml yaml = new Yaml();
        
        // zapis do pliku
        PrintWriter writer = new PrintWriter("example.yaml");
        yaml.dump(dataTree, writer);
        writer.close();
    }
}
```

Po uruchomieniu tego kodu, zostanie utworzony plik `example.yaml` z zawartością:

```yaml
name: John
age: 30
occupation: developer
```

### Wczytywanie danych z pliku YAML

Przy użyciu tej samej biblioteki możemy także wczytać dane z pliku YAML do naszej aplikacji. W poniższym przykładzie wczytujemy dane z pliku `example.yaml` i wyświetlamy je w konsoli.

```java
import org.yaml.snakeyaml.Yaml;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ReadYamlExample {

    public static void main(String[] args) throws IOException {
        // inicjalizacja obiektu YAML
        Yaml yaml = new Yaml();
        
        // wczytanie pliku
        FileReader fileReader = new FileReader("example.yaml");
        Map<String, Object> dataTree = yaml.load(fileReader);

        // wyświetlenie danych
        System.out.println("Name: " + dataTree.get("name"));
        System.out.println("Age: " + dataTree.get("age"));
        System.out.println("Occupation: " + dataTree.get("occupation"));
    }
}
```

Wynikiem działania tego kodu będzie:

```
Name: John
Age: 30
Occupation: developer
```

## Dogłębna analiza

### Struktura pliku YAML

Plik YAML składa się z par `klucz: wartość`, gdzie wartość może być dowolnym typem danych, takim jak string, liczba czy lista. Istnieje także możliwość zagnieżdżania danych, co pozwala na tworzenie bardziej złożonych struktur.

### Zalety użycia YAML

Jedną z głównych zalet YAML jest czytelność dla człowieka. Dzięki swojej składni, jest łatwiejszy w interpretacji niż na przykład format JSON. Ponadto, YAML pozwala na przekazywanie danych w postaci hierarchicznej, co może być bardzo przydatne w niektórych przypadkach.

### Biblioteki wspierające pracę z YAML

Poza biblioteką SnakeYAML, istnieje wiele innych bibliotek, które wspierają pracę z YAML w środowisku Javy. Jednym z popularnych wyborów jest biblioteka Jackson, która pozwala na seralizację i deserializację obiektów do i z formatu YAML.

## Zobacz także

- [Strona domowa formatu YAML](https://yaml.org/)
- [Dokumentacja biblioteki SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Home)
- [Przykłady użycia biblioteki Jackson z