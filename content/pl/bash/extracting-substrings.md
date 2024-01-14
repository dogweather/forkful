---
title:                "Bash: Wyodrębnianie podłańcuchów"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli zajmujesz się programowaniem w Bash lub masz w planach zacząć, to na pewno natknąłeś się na potrzebę wyodrębnienia fragmentów tekstu. To często przydatne w różnych scenariuszach, na przykład podczas przetwarzania dużych plików tekstowych lub analizy danych. W tym artykule dowiecie się, jak wykorzystać to narzędzie do ekstrakcji podciągów w Bash.

## Jak To Zrobić

Kodowanie w Bash jest proste i szybkie, dlatego osoby pracujące w tym środowisku często wybierają je do ekstrakcji podciągów. Aby wyodrębnić fragment tekstu, wystarczy użyć funkcji `cut` lub `awk`. Poniżej znajdują się przykładowe kody z wynikami dla obu tych opcji, wykorzystując polecenie `echo` do wyświetlenia tekstu.

### Przykład 1: Wykorzystanie funkcji `cut`

```
Bash echo "Lorem ipsum dolor sit amet" | cut -d ' ' -f 2-4
# Output: ipsum dolor sit
```

W powyższym przykładzie wykorzystaliśmy funkcję `cut` z argumentami `-d` (delimiter) ustawionym na spację oraz `-f` (field) ustawionym na zakres pól od drugiego do czwartego, co pozwala nam na wyodrębnienie wybranego fragmentu tekstu.

### Przykład 2: Wykorzystanie funkcji `awk`

```
Bash echo "Lorem ipsum dolor sit amet" | awk '{print $3,$4,$5}'
# Output: dolor sit amet
```

W drugim przykładzie skorzystaliśmy z funkcji `awk` z poleceniem `print` w celu wyświetlenia wybranych pól tekstu, w tym przypadku trzeciego, czwartego i piątego.

## Głębsza Analiza

Ekstrakcja podciągów jest możliwa dzięki funkcji `cut` lub `awk`, które są w stanie przetwarzać duże ilości tekstu w krótkim czasie. Aby uzyskać lepsze zrozumienie działania tych funkcji, warto zapoznać się z dodatkowymi opcjami, na przykład wybieraniem konkretnych znaków lub wykorzystaniem separatora innych niż spacja.

## Zobacz Również

1. [Dokumentacja polecenia cut](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
2. [Dokumentacja polecenia awk](https://www.gnu.org/software/gawk/manual/html_node/index.html)
3. [Wstęp do programowania w Bash](https://programminghistorian.org/en/lessons/intro-to-bash)