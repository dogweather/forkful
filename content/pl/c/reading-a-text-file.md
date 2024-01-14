---
title:    "C: Odczytywanie pliku tekstowego"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto przeczytać plik tekstowy?

Jeśli programujesz w języku C, prawdopodobnie spotkałeś się z zadaniem odczytania pliku tekstowego. Jest to bardzo przydatne w wielu sytuacjach, na przykład gdy chcesz przetworzyć duże ilości danych lub pobierać informacje z zewnętrznego pliku. W tym artykule dowiesz się, jak w prosty sposób odczytywać pliki tekstowe w języku C.

## Jak to zrobić?

Zacznijmy od przykładu. Załóżmy, że mamy plik tekstowy o nazwie "dane.txt" z następującą zawartością:

```
Imię: Jan
Nazwisko: Kowalski
Wiek: 28
```

Naszym zadaniem będzie odczytać ten plik i wyświetlić jego zawartość na ekranie. W tym celu musimy najpierw otworzyć plik za pomocą funkcji `fopen()`:

```
FILE *plik = fopen("dane.txt", "r");
```

Teraz możemy użyć funkcji `fscanf()` do odczytania kolejnych wartości z pliku. Zwróć uwagę, że używamy tu formatowania `%s` do odczytania ciągów znaków - w tym przypadku imienia i nazwiska:

```
char imie[20], nazwisko[20];
int wiek;

fscanf(plik, "Imię: %s\nNazwisko: %s\nWiek: %d", imie, nazwisko, &wiek);
```

Na końcu nie zapomnijmy zamknąć pliku za pomocą funkcji `fclose()`:

```
fclose(plik);
```

Teraz możemy wyświetlić odczytane dane na ekranie:

```
printf("Imię: %s, Nazwisko: %s, Wiek: %d\n", imie, nazwisko, wiek);
```

Po uruchomieniu programu, powinniśmy zobaczyć na ekranie:

```
Imię: Jan, Nazwisko: Kowalski, Wiek: 28
```

## Deep Dive

W powyższym przykładzie użyliśmy funkcji `fscanf()` do odczytywania danych z pliku. Jest to funkcja z biblioteki standardowej języka C, która jest odpowiedzialna za wchłanianie danych ze strumienia tekstowego. Parametr `plik` to wskaźnik na otwarty plik, a parametr `format` określa rodzaj danych, które chcemy odczytać. Następnie, w kolejnych argumentach podajemy zmienne, do których chcemy zapisać odczytane dane.

W powyższym przykładzie użyliśmy również formatowania `%d` do odczytania wartości liczbowej. Wartości liczbowe odczytywane są w taki sam sposób, jak ciągi znaków, jednak musimy przekazać adres zmiennej przy pomocy operatora `&`.

Dodatkowo, należy zwrócić uwagę na symbole `\n` w parametrze `format`. Są one potrzebne, aby odczytać kolejne linie z pliku.

## Zobacz też

- [Funkcja fopen() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Funkcja fscanf() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)
- [Funkcja fclose() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm)
- [Formatowanie w funkcji printf() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)