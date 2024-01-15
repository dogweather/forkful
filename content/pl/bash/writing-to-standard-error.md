---
title:                "Pisanie do standardowego błędu"
html_title:           "Bash: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest nieodłączną częścią programowania w Bash. Pozwala to na wygodne i szybkie wykrywanie błędów i diagnostykę problemów. 

## Jak to zrobić

Pisanie do standardowego błędu w Bash jest bardzo proste. Wystarczy użyć operatora `2>` po nazwie pliku, do którego chcemy przekierować błędy. Przykładowy kod wyglądałby tak:
```Bash
ls -l file_that_does_not_exist 2> errors.txt
```
W powyższym przykładzie, wynik polecenia `ls -l` zostanie przekierowany do standardowego wyjścia, a błędy zostaną zapisane w pliku `errors.txt`. Możemy również przekierować błędy na standardowe wyjście diagnostyczne używając operatora `2>&1`. Wtedy wszystkie informacje zostaną wyświetlone na ekranie.

## Głębszy wgląd

W Bash występują dwa główne strumienie wyjściowe: standardowe wyjście (ang. standard output) i standardowy błąd (ang. standard error). Standardowe wyjście jest używane do wyświetlania bieżącego stanu programu lub wyników działania polecenia. Natomiast standardowy błąd jest używany do wyświetlania błędów i ostrzeżeń. Dzięki oddzieleniu tych dwóch strumieni, możemy łatwo przekierować informacje diagnostyczne dla programisty i niezawodnie wyświetlać wyniki na ekranie. 

## Zobacz również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Przekierowywanie wejścia/wyjścia w Bash](https://www.hostinger.pl/pomoc/linux/przekierowanie-wejscia-wyjscia-bash)
- [Bash One-Liners Explained, Part III: All about redirections](https://catonmat.net/bash-one-liners-explained-part-three)