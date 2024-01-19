---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja daty na ciąg to proces przekształcania formatu daty w czytelny tekst. Programiści robią to, aby ułatwić zrozumienie i obsługę daty przez użytkowników.

## Jak to zrobić:
Aby przekształcić datę na ciąg, można użyć polecenia `date` w skrypcie powłoki Bash. Poniżej znajduje się przykładowy kod i wynik.

```Bash
data=$(date +"%Y-%m-%d")
echo $data
```
Wynik:
```Bash
2023-12-31
```
W tym przypadku, data jest formatowana jako "Rok-Miesiąc-Dzień".

## W głąb tematu
Historia Bash'a sięga końca lat 80-tych, kiedy to został stworzony jako darmowa alternatywa dla Bourne Shell. Co do formatowania daty, bash oferuje wiele opcji, które można dostosować do swoich potrzeb.

Istnieją jednak alternatywy dla wbudowanej funkcji `date`. Możemy na przykład użyć komendy `printf` albo skorzystać z zewnętrznych narzędzi jak `awk`.

Szczegóły implementacji poleceń są zazwyczaj ukrywane dla użytkownika, ale jeśli jesteś ciekawy, zajrzyj do man page dla `date` (wpisz `man date`), gdzie znajdziesz wszystko, co musisz wiedzieć.

## Zobacz też
1. [Bash Date Command](https://linuxize.com/post/bash-date/)
2. [Date Command In Bash](https://www.cyberciti.biz/faq/unix-linux-shell-scripting-formatting-dates/)
3. [Bash Date Format: A complete guide to formatting dates](https://phoenixnap.com/kb/bash-date-format)