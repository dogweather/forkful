---
title:                "Wyciąganie podciągów"
html_title:           "Fish Shell: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub człowiekiem zajmującym się automatyzacją zadań w systemie Linux, prawdopodobnie zetknąłeś się z potrzebą wyodrębnienia części tekstu z większego ciągu znaków. Może to być konieczne, aby przeprowadzić wybrane działania na danym tekście lub po prostu wyciągnąć pewne informacje. W tym artykule dowiecie się, jak szybko i wygodnie wyodrębnić podciągi w powłoce Fish Shell.

## Jak to zrobić

Wyodrębnianie podciągów w powłoce Fish Shell jest bardzo proste i wygodne. Wystarczy użyć wbudowanej funkcji `string sub`. Przyjmując zmienną `text` zawierającą cały tekst, następująca komenda wyodrębni podciąg od piątego do dziewiątego znaku i zapisze go do zmiennej `substring`:

```Fish Shell
set substring (string sub $text 5 9)
```

Możesz również wykorzystać tę funkcję wewnątrz pętli, aby wyodrębnić podciągi z wielu zmiennych. Na przykład, poniższy kod wyodrębni każdy podciąg z listy zmiennych `list` i wyświetli je w konsoli:

```Fish Shell
for var in $list
  echo (string sub $var 2 5)
end
```

Wyjście powyższego kodu będzie wyglądać mniej więcej tak:

```
odci
drie
jmic
```

Jednym z przydatnych parametrów funkcji `string sub` jest również możliwość wyodrębniania podciągów od końca. W przypadku, gdy chcemy wyodrębnić ostatnie 10 znaków z jakiegoś tekstu, możemy to zrobić przy użyciu ujemnych wartości indeksów. Na przykład:

```Fish Shell
set last_chars (string sub $text -10 -1)
```

Wyjście powyższego kodu będzie stanowić ostatnie 10 znaków zmiennej `text`.

## Deep Dive

Funkcja `string sub` w powłoce Fish Shell przyjmuje trzy argumenty: zmienną zawierającą tekst, indeks początkowy i indeks końcowy wyodrębnianego podciągu. Indeks pierwszego znaku tekstu to 0. Dodatkowo, jeśli podamy ujemną wartość indeksu końcowego, zostanie wyodrębniony podciąg od tej pozycji do końca tekstu.

Funkcja ta jest również bardzo przydatna, gdy chcemy wyciągnąć informacje z tekstu zawierającego wiele linii. W przypadku gdy każda linia zawiera inny format danych, możemy wyodrębnić odpowiednie wartości, np. z wcześniej wspomnianego wyjścia pętli.

## Zobacz także

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Wprowadzenie do programowania w Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Podstawy przetwarzania tekstu w powłoce Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_text_processing)