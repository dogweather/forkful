---
title:    "Python: Odczytywanie pliku tekstowego"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś/aś przeczytać plik tekstowy w Pythonie, ale nie wiedziałeś/aś, jak to zrobić? Ta krótka instrukcja pozwoli Ci zrozumieć, dlaczego jest to ważna umiejętność dla każdego programisty Pythona i jak to zrobić w prosty sposób.

## Jak to zrobić

Czytanie pliku tekstowego w Pythonie jest bardzo proste. Najpierw musimy otworzyć plik, a następnie przeczytać jego zawartość. Poniżej znajdziesz przykładowy kod, który pokazuje, jak otworzyć i przeczytać plik tekstowy w Pythonie:

```Python
with open("plik.txt", "r") as file:
    for line in file:
        print(line)
```

W powyższym przykładzie użyliśmy funkcji `open()` do otwarcia pliku tekstowego o nazwie "plik.txt". Argument "r" oznacza, że chcemy tylko czytać plik. Następnie używamy pętli `for` do iterowania przez każdą linię w pliku i wyświetlamy ją przy użyciu funkcji `print()`. W ten sposób możemy odczytać całą zawartość pliku tekstowego.

Jeśli chcesz tylko przeczytać określoną ilość linii w pliku, możesz użyć funkcji `readlines()` zamiast pętli `for`. Przykładowy kod będzie wyglądał następująco:

```Python
with open("plik.txt", "r") as file:
    lines = file.readlines()
    print(lines[0:5])
```

W powyższym przykładzie użyliśmy funkcji `readlines()` do wczytania wszystkich linii w pliku i zapisaliśmy je do zmiennej `lines`. Następnie za pomocą indeksowania wyświetliliśmy pierwszych pięć linii.

## Głębokie pogłębienie

Teraz, gdy już wiesz, jak otworzyć i przeczytać plik tekstowy w Pythonie, warto również poznać inne funkcje, które mogą Ci się przydać. Na przykład, jeśli chcesz odczytać plik w trybie binarnym, możesz użyć argumentu "rb" w funkcji `open()`. Możesz również wybrać w jakim formacie chcesz odczytywać lub zapisywać plik, korzystając z argumentu "encoding".

Jedną z przydatnych funkcji jest również `strip()`, która usuwa białe znaki z początku i końca linii. Może to być przydatne, gdy pracujesz z danymi wejściowymi, które mogą zawierać nadmiarowe białe znaki.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pracy z plikami tekstowymi w Pythonie, warto zapoznać się z oficjalną dokumentacją języka Python. Poniżej znajdziesz kilka przydatnych linków:

- [Oficjalna dokumentacja Pythona](https://docs.python.org/pl/3/tutorial/index.html)
- [Tutorial na temat pracy z plikami w Pythonie](https://www.tutorialspoint.com/python/python_files_io.htm)
- [Kurs "Python dla każdego - od podstaw"](https://kurs.gram.pl/kurs-python/)

Teraz już wiesz, jak otworzyć i przeczytać plik tekstowy w Pythonie. Mam nadzieję, że ta krótka instrukcja była dla Ciebie pomocna i zapraszam do dalszego pogłębiania wiedzy na temat programowania w Pythonie!