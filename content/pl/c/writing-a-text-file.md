---
title:    "C: Pisanie pliku tekstowego"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie tekstowego pliku jest kluczowym elementem w wielu programach. Pozwala nam zapisywać i przechowywać dane, a także udostępniać je innym użytkownikom. Bez możliwości tworzenia i modyfikowania plików tekstowych, nasze programy straciłyby wiele ze swojej funkcjonalności.

## Jak to zrobić

Tworzenie tekstowych plików w języku C jest bardzo proste. Najważniejszą funkcją, która pozwala nam na to, jest funkcja `fopen()`. Musimy podać jej dwa argumenty: nazwę pliku, który chcemy stworzyć oraz tryb, w jakim chcemy go otworzyć. Przykładowy kod wyglądałby następująco:

```
FILE *plik; // deklaracja zmiennej plikowej
plik = fopen("moj_plik.txt", "w"); // otwarcie pliku w trybie zapisu
```

Następnie możemy użyć funkcji `fprintf()` aby zapisać dowolny tekst do naszego pliku. Przykładowy kod wykorzystujący tę funkcję wyglądałby tak:

```
fprintf(plik, "To jest przykładowy tekst, który zostanie zapisany do pliku.");
```

Po zakończeniu zapisu, musimy pamiętać o zamknięciu pliku przy użyciu funkcji `fclose()`:

```
fclose(plik); // zamknięcie pliku
```

Po wykonaniu tych kroków, nasz plik tekstowy zostanie stworzony i będzie zawierał wprowadzony przez nas tekst. Możemy również odczytać dane z pliku przy użyciu funkcji `fscanf()`.

## Głębsze zagadnienia

Kiedy zaczynamy pisać bardziej skomplikowane programy, często chcemy aby nasz kod był czytelniejszy i łatwiejszy w modyfikacji. W takich przypadkach przydatne może być wykorzystanie funkcji `fgetc()` i `fgets()`, które pozwalają na odczyt pojedynczych znaków lub całych linii z pliku tekstowego. Możemy również wykorzystać funkcje `fseek()` i `ftell()` aby przechodzić pomiędzy różnymi miejscami w pliku oraz sprawdzić jego rozmiar.

## Zobacz także

- Dokładna dokumentacja funkcji `fopen()` w języku C: https://www.cplusplus.com/reference/cstdio/fopen/
- Przykładowe programy wykorzystujące pisanie i odczytywanie plików tekstowych w języku C: https://www.geeksforgeeks.org/c-programming-language/?ref=leftbar-rightbar