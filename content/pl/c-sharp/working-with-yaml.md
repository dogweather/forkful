---
title:                "C#: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego?

Dlaczego ktoś powinien zainteresować się pracą z YAML? Jest to aktualnie jedna z najpopularniejszych form formatowania tekstu, która jest wykorzystywana w wielu językach programowania, w tym również w C#. Ponadto jest to bardzo prosty i czytelny sposób na przechowywanie danych, co pozwala na łatwą komunikację między różnymi systemami.

## Jak to zrobić?

### Tworzenie pliku YAML

Pierwszym krokiem jest utworzenie pliku YAML w Twoim projekcie C#. W tym celu wystarczy kliknąć prawym przyciskiem myszy na nazwę projektu w Visual Studio, wybrać opcję "Dodaj" i następnie "Nowy element". Wybierz "Plik YAML" i nadaj mu odpowiednią nazwę.

### Struktura pliku YAML

Plik YAML składa się z kluczy i wartości, które są od siebie oddzielone dwukropkiem. Przykładowa struktura może wyglądać tak:

```C#
klucz: wartość
inna_klucz: inna_wartość
```

### Wczytywanie pliku YAML

Aby wczytać dane znajdujące się w pliku YAML, należy skorzystać z odpowiedniej biblioteki, na przykład yaml-dotnet. W poniższym przykładzie wczytujemy dane i wyświetlamy je na konsoli:

```C#
using System;
using YamlDotNet.Serialization;
 
public class Program 
{
    public static void Main() 
    {
        var input = @"
            jedzenie:
              - pizza
              - hamburgery
              - sushi
            napoje:
              - cola
              - woda
              - sok pomarańczowy
        ";
 
        Deserializer deserializer = new DeserializerBuilder().Build();
        var menu = deserializer.Deserialize<Dictionary<string, List<string>>>(input);
 
        foreach (var category in menu) 
        {
            Console.WriteLine(category.Key + ": ");
            foreach (var item in category.Value) 
            {
                Console.WriteLine(" " + item);
            }
        }
    }
}
```

### Wynik

```
jedzenie:
  pizza
  hamburgery
  sushi
napoje:
  cola
  woda
  sok pomarańczowy
```

## Deep Dive

Istnieje wiele różnych sposobów na pracę z YAML w C#, w tym modyfikowanie i zapisywanie danych oraz tworzenie własnych obiektów. Warto również zwrócić uwagę na narzędzia, takie jak Visual Studio Code czy YAML Lint, które ułatwiają pracę z tym formatem. Pamiętaj również o aktualizowaniu bibliotek zależnych, aby uniknąć błędów i utrzymać kompatybilność z najnowszymi wersjami YAML.

## Zobacz też

- [Dokumentacja YAML](https://yaml.org/spec/)
- [yaml-dotnet](https://github.com/aaubry/YamlDotNet)
- [Visual Studio Code](https://code.visualstudio.com/)
- [YAML Lint](https://www.yamllint.com/)