---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "C#: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & dlaczego?

Sprawdzanie czy istnieje katalog jest jedną z najważniejszych czynności programistycznych, które może wydawać się niepozorna, ale jest niezbędna w wielu projektach. Programiści często sprawdzają czy katalog istnieje, aby upewnić się, że aplikacja będzie działać poprawnie i będzie w stanie znaleźć potrzebne pliki.

## Jak to zrobić:

```C#
if (Directory.Exists(path))
{
    Console.WriteLine("Katalog istnieje.");
}
```

W powyższym przykładzie sprawdzamy czy katalog o podanej ścieżce istnieje, a następnie wypisujemy informację na ekranie. 

## Głębsze wgląd:

Sprawdzanie czy katalog istnieje jest powszechne od lat w programowaniu, ponieważ programiści muszą dbać o to, aby ich aplikacje były odporne na błędy i potrafiły reagować na różne sytuacje. Istnieją także alternatywne metody sprawdzania istnienia katalogu, takie jak korzystanie z biblioteki System.IO lub wywoływanie odpowiedniej komendy w konsoli.

W implementacji sprawdzania czy katalog istnieje, program najpierw musi uzyskać dostęp do systemu operacyjnego, aby sprawdzić czy istnieje dany katalog. Następnie, jeśli katalog istnieje, zostanie zwrócona wartość true, a w przeciwnym przypadku - false.

## Zobacz także:

Dokumentacja Microsoft dotycząca sprawdzania istnienia katalogu w języku C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory.exists