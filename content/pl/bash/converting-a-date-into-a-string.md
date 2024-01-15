---
title:                "Konwertowanie daty na ciąg znaków."
html_title:           "Bash: Konwertowanie daty na ciąg znaków."
simple_title:         "Konwertowanie daty na ciąg znaków."
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy przekonwertować datę na ciąg znaków, na przykład do wyświetlenia jej w czytelniejszej formie lub do dalszej obróbki. W tym artykule dowiesz się, jak wykonać tę czynność w Bashu.

## Jak to zrobić

Bash ma wbudowane wiele funkcji, w tym `date`, która pozwala na wyświetlanie bieżącej daty i czasu. Aby przekonwertować datę na ciąg znaków, należy użyć opcji `+FORMAT`, gdzie FORMAT określa pożądany format daty w formie ciągu znaków.

```Bash
date +"%d-%m-%Y %H:%M:%S"
```
Wyjście powyższego polecenia będzie wyglądać następująco: `18-09-2021 13:37:00`. Warto zauważyć, że formatowanie daty jest podobne do tego, które stosuje się w języku C.

Możemy również zastosować inne opcje, na przykład `--date` do przekonwertowania innej daty niż bieżąca lub `-d` do przekonwertowania daty przekazanej przez użytkownika.

```Bash
date --date="28 August 2021" +"%A"
```
Wyjście to `Saturday`, ponieważ 28 sierpnia 2021 to sobota.

Możemy także użyć opcji `-f` do przekonwertowania daty z formatu innego niż domyślny na ciąg znaków.

```Bash
date -f "%s" 1631935225
```
Wyjście to `Sat Sep 18 13:33:45 CEST 2021`, ponieważ data w systemie jest przechowywana w postaci liczby sekund od 1 stycznia 1970 roku.

Te przykłady pokazują podstawowe wykorzystanie `date` do konwersji daty na ciąg znaków, ale są również inne opcje, które można wykorzystać do bardziej zaawansowanych zadań.

## Głębsza analiza

Bash używa standardowych formatów dat z języka C, jak również oferuje możliwość tworzenia własnych formatów za pomocą opcji `date` i `printf`. Więcej informacji na ten temat można znaleźć w dokumentacji systemowej Bash.

Konwertowanie daty na ciąg znaków jest również wykorzystywane w skryptach do tworzenia plików lub folderów z datą w nazwie, co ułatwia archiwizację plików lub monitorowanie dat utworzenia.

## Zobacz także

- [Dokumentacja systemowa Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Formatowanie dat w Bashu z wykorzystaniem printf](https://www.devdungeon.com/content/take-date-bash-unix)
- [Przetwarzanie dat w Bashu](https://blog.sleeplessbeastie.eu/2016/09/05/how-to-process-dates-in-bash/)