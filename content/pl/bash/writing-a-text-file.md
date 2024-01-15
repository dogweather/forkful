---
title:                "Tworzenie pliku tekstowego"
html_title:           "Bash: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią programowania w Bashu. Jest to nie tylko użyteczna umiejętność do posiadania, ale także niezbędna, jeśli chcesz tworzyć skrypty Bash, które są w stanie przetwarzać i manipulować danymi.

## Jak to zrobić

Aby stworzyć nowy plik tekstowy w Bashu, użyj polecenia `touch` w następujący sposób:

```Bash
touch nowy_plik.txt
```

Powyższa komenda tworzy nowy plik o nazwie "nowy_plik.txt". Aby edytować ten plik tekstowy, możesz użyć dowolnego edytora tekstu, takiego jak `nano` lub `vim`. Na przykład, aby otworzyć plik w `nano`, możesz użyć polecenia:

```Bash
nano nowy_plik.txt
```

Po zakończeniu edycji pliku tekstowego, zapisz go używając kombinacji klawiszy `Ctrl+X`, a następnie zatwierdź zmiany klikając `y` i naciskając `Enter`.

## Zagłębienie

W Bashu istnieje wiele komend i funkcji, które można wykorzystać do pisania plików tekstowych. Jednym z przydatniejszych poleceń jest `echo`, które pozwala wypisywać treść tekstu lub zmiennych do pliku tekstowego. Na przykład:

```Bash
echo "To jest przykładowy tekst" > nowy_plik.txt
```

Powyższa komenda wypisze "To jest przykładowy tekst" do pliku tekstowego "nowy_plik.txt". Innym przydatnym poleceniem jest `cat`, które służy do wyświetlania zawartości pliku. Na przykład:

```Bash
cat nowy_plik.txt
```

Spowoduje wyświetlenie zawartości pliku "nowy_plik.txt" w terminalu.

## Zobacz także

- [Bash - Oficjalna strona](https://www.gnu.org/software/bash/)
- [Kurs Bash - Tłumaczenie dokumentacji GNU](https://www.tldp.org/LDP/abs/html/)
- [BashGuide - Przewodnik po skryptach Bash](http://mywiki.wooledge.org/BashGuide)