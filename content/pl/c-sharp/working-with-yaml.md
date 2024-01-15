---
title:                "Praca z yaml"
html_title:           "C#: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą w języku C# i często pracujesz z konfiguracją aplikacji, to z pewnością zetknąłeś się z plikami YAML. YAML jest formatem danych, który jest coraz bardziej popularny w świecie programowania, ponieważ jest czytelny dla ludzi i łatwy do przetwarzania przez komputery. To dlatego warto poznać go bliżej i wykorzystywać w swoich projektach.

## Jak to zrobić

Aby pracować z YAML w języku C#, musimy najpierw zainstalować odpowiednią bibliotekę. Możemy to zrobić poprzez menedżera pakietów NuGet lub ręcznie dodać referencję do projektu.

Następnie, aby zacząć parsować pliki YAML, musimy utworzyć instancję obiektu `YamlDotNet.Core.YamlStream`. Przyjmie on jako argument strumień pliku lub ciąg znaków zawierający dane YAML.

```C#
var input = @"
 - name: John
   age: 25
 - name: Emily
   age: 30
";
var stream = new YamlStream();
stream.Load(new StringReader(input));
```

Teraz możemy uzyskać dostęp do danych z pliku YAML. Na przykład, jeśli chcemy pobrać imię i wiek pierwszej osoby z listy, możemy to zrobić w następujący sposób:

```C#
var name = (string) stream.Documents[0].RootNode["name"];
var age = (int) stream.Documents[0].RootNode["age"];
```

Podobnie, możemy również tworzyć pliki YAML poprzez tworzenie obiektów `YamlMappingNode` i `YamlScalarNode`. Na przykład, aby stworzyć prosty plik YAML, możemy użyć kodu:

```C#
var mapping = new YamlMappingNode();
mapping.Add("name", "Mark");
mapping.Add("age", 35);

var output = new YamlStream(mapping);
```

Kod ten utworzy następujący plik YAML:

```yaml
name: Mark
age: 35
```

## Głębsze zagadnienia

Istnieje wiele innych możliwości pracy z YAML w języku C#, takich jak możliwość walidacji danych, obsługa kolekcji i niestandardowych typów czy także serializacja obiektów do formatu YAML. Aby dowiedzieć się więcej, polecam zapoznać się z dokumentacją biblioteki YamlDotNet lub szukać tutoriali i przykładów w internecie.

## Zobacz też

- [Oficjalna dokumentacja YamlDotNet](https://github.com/aaubry/YamlDotNet/wiki)
- [Przykładowy projekt wykorzystujący YAML w C#](https://github.com/markrogersjr/Yaml-Example)