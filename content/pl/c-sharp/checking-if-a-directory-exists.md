---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "C#: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy sprawdzać, czy dany folder istnieje, aby móc poprawnie wykonywać nasz kod. W artykule tym dowiesz się, jak to zrobić w prosty i efektywny sposób w języku C#.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje w C# jest bardzo proste. Wystarczy użyć metody "Directory.Exists()", podając jako argument ścieżkę do katalogu, który chcemy sprawdzić. Poniżej znajduje się przykładowy kod, który wykorzystuje tę metodę:

```C#
if(Directory.Exists("C:\\Users\\Username\\Documents"))
{
    Console.WriteLine("Katalog istnieje!");
}
else
{
    Console.WriteLine("Katalog nie istnieje.");
}
```

Jeżeli katalog istnieje, w konsoli pojawi się napis "Katalog istnieje!", w przeciwnym wypadku zostanie wyświetlony napis "Katalog nie istnieje.".

## Deep Dive

Głównym elementem używanym do sprawdzania istnienia katalogu jest klasa "Directory" z przestrzeni nazw "System.IO". Oprócz metody "Exists()", dostępne są również inne przydatne funkcje, takie jak "GetCreationTime()" czy "GetLastAccessTime()", które pozwalają na pobranie daty utworzenia i ostatniego dostępu do katalogu.

Warto również pamiętać, że metoda "Exists()" zwraca wartość logiczną (typ bool), więc może być bezpośrednio użyta w warunkach if-else, jak w powyższym przykładzie.

## Zobacz także

- [Dokumentacja C# - klasa Directory](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory?view=net-5.0)
- [Poradnik wideo: Jak sprawdzić, czy katalog istnieje w C#](https://www.youtube.com/watch?v=LMF5aISD04g)
- [Inne przydatne porady i triki z języka C#](https://devstyle.pl/category/c-sharp/)