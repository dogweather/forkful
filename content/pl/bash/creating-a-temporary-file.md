---
title:    "Bash: Tworzenie pliku tymczasowego"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest nieodzownym elementem większości programów w Bash. Są one wykorzystywane do przechowywania danych tymczasowych, np. wyników działania skryptu lub przypisania zmiennych. Tworzenie tych plików jest nie tylko wygodne, ale także pomaga w utrzymaniu porządku w systemie plików.

## Jak to zrobić

Tworzenie plików tymczasowych w Bash jest bardzo proste i wymaga użycia jednego polecenia - `mktemp`. Poniżej znajduje się przykład kodu, który tworzy tymczasowy plik i wpisuje do niego nazwę użytkownika, a następnie wyświetla jego zawartość:

```Bash
#!/bin/bash

temp_file=$(mktemp) # tworzy tymczasowy plik i przypisuje jego nazwę do zmiennej temp_file
echo "Nazwa użytkownika: $USER" > "$temp_file" # wpisuje nazwę użytkownika do pliku
cat "$temp_file" # wyświetla zawartość pliku
```

Po uruchomieniu skryptu, powinniśmy otrzymać następujący wynik:

```
Nazwa użytkownika: jankowalski
```

Plik tymczasowy zostanie automatycznie usunięty po zakończeniu działania skryptu. Jest to bardzo wygodne rozwiązanie, ponieważ nie musimy pamiętać o ręcznym usuwaniu plików tymczasowych.

## Głębszy wniosek

Warto wiedzieć, że polecenie `mktemp` posiada wiele opcji, które pozwalają dostosować zachowanie tworzonego pliku tymczasowego. Możemy na przykład wybrać, w jakim miejscu ma zostać utworzony plik (domyślnie w katalogu /tmp), określić jego nazwę lub włączyć automatyczne usuwanie pliku po określonej liczbie dni. Szczegóły można znaleźć w dokumentacji polecenia.

Ponadto, warto pamiętać o używaniu polecenia `mktemp` w celu uniknięcia konfliktów z nazwami plików. Tworzenie plików tymczasowych w ten sposób zapewnia, że nie będzie możliwości nadpisania istniejącego pliku.

## Zobacz też

- [Dokumentacja polecenia mktemp (man pages)](https://linux.die.net/man/1/mktemp)
- [Artykuł o tworzeniu plików tymczasowych](https://www.folkstalk.com/2013/03/create-temporary-file-in-linux-unix.html) (po angielsku)