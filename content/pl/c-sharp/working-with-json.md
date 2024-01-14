---
title:                "C#: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest bardzo popularnym formatem danych w dzisiejszych czasach. Jest to język niezależny, lekki i łatwy w użyciu, który pozwala na wymianę danych między różnymi platformami i językami programowania. W artykule tym dowiesz się, dlaczego warto poznać i pracować z JSON w języku C#.

## Jak To Zrobić

Aby wykorzystać JSON w swoich projektach C#, możesz skorzystać z biblioteki Newtonsoft.JSON, która jest dostępna poprzez NuGet. Aby rozpocząć pracę z tym formatem danych, należy najpierw zdekodować dane JSON do odpowiednich typów obiektów w C#.

Przykładowo, jeśli chcesz odczytać spis pracowników w formacie JSON, można to zrobić w następujący sposób:

```C#
// Przygotowanie danych JSON jako string
string json = @"{
    'pracownicy': [
        { 'imie': 'Jan', 'nazwisko': 'Kowalski', 'wiek': '30' },
        { 'imie': 'Anna', 'nazwisko': 'Nowak', 'wiek': '28' },
        { 'imie': 'Katarzyna', 'nazwisko': 'Wójcik', 'wiek': '33' }
    ]
}"; 

// Dekodowanie do odpowiednich typów obiektów za pomocą metody DeserializeObject
List<Pracownik> pracownicy = JsonConvert.DeserializeObject<List<Pracownik>>(json);

// Przykładowa klasa Pracownik
public class Pracownik
{
    public string Imie { get; set; }
    public string Nazwisko { get; set; }
    public int Wiek { get; set; }
}

// Przykładowe wyświetlenie danych
foreach (var pracownik in pracownicy)
{
    Console.WriteLine($"{pracownik.Imie} {pracownik.Nazwisko}, {pracownik.Wiek} lat");
}

// Output:
// Jan Kowalski, 30 lat
// Anna Nowak, 28 lat
// Katarzyna Wójcik, 33 lat
```

## Deep Dive

JSON jest bardzo elastycznym formatem danych, który pozwala na zapisanie różnych typów danych, takich jak liczby, tekst, listy czy obiekty. W języku C# możemy również dodatkowo dostosować sposób deserializacji poprzez wykorzystanie atrybutów, np. [JsonProperty], które pozwalają na zmianę nazw pól w deserializowanym obiekcie.

Dodatkowo, można również użyć metody SerializeObject, aby przekonwertować obiekty C# na dane JSON, co może być przydatne przy tworzeniu, np. API, które wymieniają się informacjami w tym formacie.

## Zobacz również

- [Dokumentacja biblioteki Newtonsoft.Json](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [Pełny tutorial o pracy z JSON w C#](https://www.c-sharpcorner.com/article/json-parsing-and-serialization-in-c-sharp/)
- [Codecademy kurs o pracy z JSON w C#](https://www.codecademy.com/learn/learn-json-in-csharp)