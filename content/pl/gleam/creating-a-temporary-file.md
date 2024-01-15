---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Gleam: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, pewnie już wiesz, że praca z plikami jest nieodłączną częścią tworzenia oprogramowania. Czasem jednak potrzebujemy dostępu do pliku tylko na chwilę, bez konieczności jego trwałego przechowywania. W tym przypadku korzystamy z tzw. plików tymczasowych, które są idealnym rozwiązaniem dla tego typu potrzeb. W tym artykule opowiemy o tym, jak w języku Gleam stworzyć i korzystać z plików tymczasowych, a także dlaczego warto z nich korzystać.

## Jak To Zrobić

Stworzenie pliku tymczasowego w języku Gleam jest bardzo proste. Możemy to zrobić przy użyciu modułu `Tempfile`. Poniżej znajduje się przykładowy kod, który stworzy plik tymczasowy i wypisze jego zawartość:

```Gleam
import Tempfile

let temp_file = Tempfile.create()

IO.write(temp_file, "To jest zawartość pliku tymczasowego!")

let content = IO.read(temp_file)

assert content == "To jest zawartość pliku tymczasowego!"
```

W pierwszej linii kodu importujemy moduł `Tempfile`, który zawiera funkcje do pracy z plikami tymczasowymi. Następnie, przy użyciu funkcji `create()` tworzymy plik tymczasowy, który jest automatycznie usuwany po zakończeniu programu. Możemy również użyć funkcji `create_with()` aby określić prefiks nazwy pliku.

W drugiej części kodu przekazujemy treść, którą chcemy zapisać do pliku tymczasowego, przy użyciu funkcji `write()`. Następnie, przy użyciu funkcji `read()` odczytujemy zawartość pliku i przypisujemy ją do zmiennej `content`.

Na koniec, przy użyciu funkcji `assert` sprawdzamy, czy zawartość pliku jest taka sama, jaką wcześniej zapisaliśmy.

## Głębsze Zanurzenie

Pliki tymczasowe są szczególnie przydatne w przypadku pracy z dużymi i skomplikowanymi danymi, takimi jak bazy danych czy pliki graficzne. Poza tym, są również przydatne w testowaniu i debugowaniu aplikacji, ponieważ nie zanieczyszczają naszego projektu stałymi plikami.

Warto również zwrócić uwagę na fakt, że pliki tymczasowe są tworzone w systemowym katalogu tymczasowym. W systemie Windows jest to zazwyczaj folder `Temp`, a w systemach Unixowych `tmp`. Należy pamiętać, że pliki tymczasowe nie są odporne na awarie systemu i mogą zostać usunięte przez system operacyjny w trakcie działania aplikacji.

## Zobacz Również

Jeśli jesteś zainteresowany dalszą lekturą na temat plików tymczasowych w języku Gleam, polecamy zapoznać się z dokumentacją [modułu Tempfile](https://gleam.run/modules/tempfile/) oraz przeczytać artykuł [Working with temporary files in Gleam](https://rasheduddin.me/working-with-temporary-files-in-gleam/).