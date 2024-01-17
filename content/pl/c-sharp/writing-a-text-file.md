---
title:                "Pisanie pliku tekstowego"
html_title:           "C#: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zapisywanie pliku tekstowego to proces, w którym programiści mogą zapisać informacje w formacie tekstowym na swoim komputerze. Jest to przydatne, ponieważ pozwala programom odczytać i wykorzystać te informacje w przyszłości. Programiści często będą zapisywać pliki tekstowe w celu przechowywania ustawień, wyników lub innych ważnych danych.

## Jak to zrobić:
Aby zapisać plik tekstowy w C#, należy najpierw zadeklarować zmienną typu `StreamWriter`. Następnie należy użyć metody `WriteLine()` lub `Write()`, aby zapisać tekst w pliku. Na przykład:

```C#
using System;
using System.IO;

StreamWriter plikTekstowy = new StreamWriter("plik.txt");
plikTekstowy.WriteLine("To jest przykładowy tekst do zapisania w pliku.");
plikTekstowy.Close();
```

Ten kod spowoduje utworzenie pliku tekstowego o nazwie "plik.txt" i zapisze w nim linię tekstu.

## Deep Dive:
Zapisywanie plików tekstowych w programowaniu jest już powszechną praktyką od wielu lat. Przed pojawieniem się graficznych interfejsów użytkownika, programiści często korzystali z plików tekstowych, aby przechowywać dane w formie ustrukturyzowanej.

Alternatywnym sposobem na przechowywanie danych jest korzystanie z baz danych, jednak pliki tekstowe są wciąż często wykorzystywane w niewielkich projektach lub do przechowywania prostych informacji.

Gdy w pliku tekstowym zapisywane są dane, są one zapisywane w postaci ciągu znaków. W przypadku bardziej skomplikowanych danych, należy skorzystać z formatów danych, takich jak XML lub JSON.

## Zobacz także:
Zapisywanie plików tekstowych jest tylko jednym z elementów pracy z plikami w C#. Warto również zwrócić uwagę na odczytywanie plików tekstowych oraz modyfikowanie istniejących plików. Więcej informacji na ten temat można znaleźć w dokumentacji Microsoft na temat klas `StreamWriter` i `StreamReader`.