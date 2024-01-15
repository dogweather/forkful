---
title:                "Drukowanie wyjścia debugowania"
html_title:           "Bash: Drukowanie wyjścia debugowania"
simple_title:         "Drukowanie wyjścia debugowania"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyświetlanie informacji debugujących jest ważnym narzędziem dla programistów w pracy z językiem Bash. Pozwala to na wgląd w działanie skryptów oraz diagnozowanie ewentualnych błędów.

## Jak to zrobić

Aby wyświetlić informacje debugujące w języku Bash, należy użyć polecenia `set -x`, które przełącza tryb debugowania. Następnie, przed linijką kodu, którą chcemy prześledzić, dodajemy prefiks `+` lub `x` w zależności od wersji Bash, które będziemy używali.

```Bash
set -x

# + lub x przed każdą linijką, która ma być wyświetlona
echo "Cześć, świecie!"
```

Powyższy przykład spowoduje wyświetlenie informacji debugujących przy wywołaniu komendy `echo`, co pozwoli nam na zobaczenie, czy zmienna została prawidłowo zdefiniowana czy też czy występują jakieś inne błędy.

## W głębi

Polecenie `set -x` włącza tryb debugowania na poziomie skrypty. Oznacza to, że wszystkie linie kodu będą wyświetlane w terminalu, co może spowodować przeładowanie informacją. Można jednak również włączyć tryb debugowania tylko dla konkretnego fragmentu kodu, używając polecenia `set +x` w celu wyłączenia trybu debugowania.

Inną przydatną opcją jest użycie polecenia `set -v`, które wyświetla informacje debugujące, ale także wypisuje linie kodu, które są w danej chwili wykonywane. Może to być szczególnie przydatne przy pracy z pętlami, aby śledzić kolejność wykonywanych komend.

## Zobacz także

- [Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Przydatne porady dotyczące debugowania w Bashu](https://www.lifewire.com/debug-code-in-bash-script-2200574)
- [Artykuł o wykorzystaniu polecenia `set -x` w Bashu](https://www.baeldung.com/linux/enable-set-x-scripting)