---
title:    "C#: Pisanie pliku tekstowego"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Dlaczego pisać plik tekstowy?

Pisanie plików tekstowych jest nieodłączną częścią programowania w C#. Jest to niezbędny krok podczas tworzenia aplikacji, gdy chcesz przechowywać dane w trwałej formie lub przesyłać je do innych aplikacji. Pliki tekstowe są również niezwykle użyteczne w przypadku przetwarzania dużych ilości danych.

## Jak to zrobić?

Aby zapisać dane do pliku tekstowego w języku C#, potrzebujesz użyć klasy StreamWriter. Najpierw musisz utworzyć nowy obiekt StreamWriter, przekazując mu ścieżkę dostępu do pliku oraz wybrany tryb dostępu (np. do odczytu, zapisu lub modyfikowania). Następnie możesz użyć metody WriteLine, aby zapisać dane do pliku, a na końcu zamknąć strumień, używając metody Close.

```C#
StreamWriter writer = new StreamWriter(@"C:\Users\Użytkownik\Pulpit\plik.txt", true);
writer.WriteLine("To jest przykładowy tekst.");
writer.Close();
```

Powyższy kod utworzy nowy plik tekstowy o nazwie "plik.txt" na pulpicie i zapisze w nim wiersz tekstu "To jest przykładowy tekst." Ustawienie drugiego argumentu metody StreamWriter na true oznacza, że nowe dane będą dodawane do istniejącego pliku, a nie nadpisane.

## Głębsze zanurzenie

Istnieje wiele różnych metod i funkcji, które można wykorzystać do pisania plików tekstowych w języku C#. Niektóre z nich to:

- Write - pozwala na zapisanie pojedynczego znaku lub ciągu znaków do pliku.
- AppendText - otwiera plik do zapisu i zwraca obiekt StreamWriter.
- WriteAsync - wykonuje operację zapisu asynchronicznie.
- Path - zawiera wiele przydatnych metod do zarządzania ścieżkami dostępu do pliku.

Pamiętaj również, że przy pisaniu do pliku tekstowego istnieje ryzyko wystąpienia wyjątków, takich jak IOException lub UnauthorizedAccessException. Dlatego zawsze warto umieścić kod w bloku try-catch, aby odpowiednio obsłużyć te błędy.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pisaniu plików w języku C#, polecamy zapoznać się z poniższymi artykułami:

- Oficjalna dokumentacja klasy StreamWriter (https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamwriter)
- Wprowadzenie do obsługi plików i katalogów w języku C# (https://www.c-sharpcorner.com/UploadFile/mahakg1985/handling-files-and-directories-in-C-Sharp/)
- Wykonywanie operacji na plikach przy użyciu klasy File w języku C# (https://www.educba.com/csharp-file-class/)

Dziękujemy za przeczytanie tego artykułu i mamy nadzieję, że teraz masz lepsze pojęcie o pisaniu plików tekstowych w języku C#. Zachęcamy również do eksperymentowania z różnymi metodami i funkcjami oraz dostosowywania ich do swoich potrzeb.