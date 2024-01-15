---
title:                "Ekstrakcja podciągów"
html_title:           "C#: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programowanie jest Twoją pasją, prawdopodobnie lubisz eksplorować różne funkcje języków programowania, a dziś chcielibyśmy przyjrzeć się ekstrakcji podciągów w języku C#. Jest to przydatna umiejętność, która może pomóc Ci w pracy z tekstem i wykorzystać go w różnych zastosowaniach.

## Jak to zrobić

Aby wyciągnąć podciąg z ciągu tekstowego w C#, musimy użyć metody `Substring()`. Przyjmujemy dwa parametry - indeks początkowy i długość podciągu, który chcemy uzyskać. Przykładowy kod wyglądałby tak:

```C#
string tekst = "Lorem ipsum dolor sit amet";

string podciag = tekst.Substring(6, 5); // wynikiem będzie "ipsum"
```

Pamiętaj, że indeksowanie w C# zaczyna się od zera, więc pierwszy znak w ciągu ma indeks 0.

Jeśli chcesz wyciągnąć podciąg od danego indeksu do końca ciągu, możesz pominąć drugi parametr:

```C#
string tekst = "Lorem ipsum dolor sit amet";

string podciag = tekst.Substring(12); // wynikiem będzie "dolor sit amet"
```

Możemy także wyciągnąć podciąg od danego indeksu do końca ciągu, określając tylko długość:

```C#
string tekst = "Lorem ipsum dolor sit amet";

string podciag = tekst.Substring(0, 5); // wynikiem będzie "Lorem"
```

Pamiętaj, że jeśli podamy indeks większy niż długość ciągu, zostanie zwrócony błąd. Możemy także użyć metody `Length` aby uzyskać długość ciągu i tym samym uniknąć błędu.

## Deep Dive

Metoda `Substring()` wykonuje wiele działań za nas, co warto wiedzieć. Otóż w tle odbywa się sprawdzanie poprawności indeksów i tworzony jest nowy obiekt `string`, który jest kopią podciągu z oryginalnego tekstu.

Dzięki temu, jeśli chcesz pracować z dużymi danymi i wyciągać wiele podciągów, możesz to robić wydajniej, tworząc od razu obiekt `string` i później korzystać z niego przy użyciu metody `Substring()`. Dzięki temu unikniesz wielokrotnego tworzenia nowych obiektów i usprawnisz swoje działania.

## Zobacz także

- Dokumentacja Microsoft o metodzie `Substring()`: <https://docs.microsoft.com/pl-pl/dotnet/api/system.string.substring>
- Przykłady użycia metody `Substring()` w C#: <https://www.tutorialspoint.com/substring-method-in-chash-net>
- Wykorzystanie ekstrakcji podciągów w praktyce: <https://www.codeproject.com/Articles/1084535/Extracting-Substrings-from-a-String-NET-String-Po>