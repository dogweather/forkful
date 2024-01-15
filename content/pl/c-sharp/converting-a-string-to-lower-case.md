---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "C#: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się, dlaczego czasami musisz konwertować stringi na małe litery? To bardzo przydatna umiejętność w programowaniu, ponieważ pozwala na łatwiejsze działanie na danych oraz poprawne porównywanie i sortowanie tekstów.

## Jak to zrobić

Konwersja stringa na małe litery w C# jest bardzo prosta. Wystarczy użyć metody `ToLower()` na obiekcie `String`. Poniżej znajduje się przykładowy kod, który pokazuje, jak to zrobić:

```C#
string imie = "ADAM";
Console.WriteLine(imie.ToLower());
```

W tym przypadku, wyjściem będzie `adam`, ponieważ wszystkie litery zostały zamienione na małe.

## Głębsze spojrzenie

Konwersja stringa na małe litery w rzeczywistości polega na zmianie kodów znaków na odpowiednie kody ASCII lub Unicode, w zależności od wykorzystanej metody. Dlatego, jeśli używasz innych języków niż angielski, pamiętaj, że wynik może się różnić.

Można również zastosować metody `ToLowerInvariant()` lub `ToLower(CultureInfo)`, aby określić język, w którym nastąpi konwersja. Metoda `ToLowerInvariant()` nie jest zależna od ustawień regionalnych i zawsze zwraca wynik zgodny z językiem angielskim.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o metodach i właściwościach związanych z konwersją stringów w C#, polecam lekturę oficjalnej dokumentacji Microsoft na temat klasy `String`.

[https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)

Możesz także sprawdzić nasz poprzedni artykuł o manipulacji stringami w C#. [LINK DO ARTYKUŁU]