---
title:    "Bash: Pisanie do standardowego błędu"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego?

Niektórzy z was mogli zadać sobie pytanie: "Po co pisać na standardowe wyjście błędu w Bash?" Cóż, odpowiedź jest prosta - pisanie na standardowe wyjście błędu jest bardzo przydatne w niektórych sytuacjach. Jest to sposób na informowanie użytkownika o błędach lub ostrzeżeniach związanych z wykonywanym kodem. Dzięki temu możemy łatwiej debugować nasze skrypty i uniknąć nieprzewidzianych problemów.

## Jak to zrobić?

Aby pisać na standardowe wyjście błędu w Bash, musimy użyć polecenia `echo` wraz z opcją `-e` oraz przekierować jego wynik na standardowe wyjście błędu. Przykładowy kod może wyglądać tak:

```
#!/bin/bash
echo -e "To jest błąd!" >&2
```
W powyższym przykładzie, komenda `echo` jest użyta do wypisania informacji na standardowe wyjście błędu, a dzięki przekierowaniu wyniku na `>&2`, wiadomość ta nie pojawi się na standardowym wyjściu.

Natomiast jeśli chcemy przetestować nasz kod, możemy go uruchomić i przetestować na przykład tak:

```
./script.sh 2> error.log
```
W ten sposób przekierowujemy standardowe wyjście błędu do pliku `error.log`, który zawierać będzie nasz komunikat.

## Głębszy wgląd

Istnieje również inny sposób na pisanie na standardowe wyjście błędu w Bash - używając polecenia `printf`. Ta komenda jest bardziej rozbudowana i pozwala nam na bardziej zaawansowane formatowanie tekstu. Przykładowy kod wyglądałby tak:

```
#!/bin/bash
printf "%s\n" "To jest błąd!" >&2

```
Kod ten wyświetli to samo co przykład z użyciem `echo`, ale daje nam więcej możliwości w zakresie formatowania tekstu. Więcej informacji na temat `printf` można znaleźć w dokumentacji Bash.

## Zobacz także

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html) 
- [Przekierowanie standardowego wyjścia błędu](https://www.tldp.org/LDP/abs/html/io-redirection.html#ERRORREDIR) 
- [Polecenie printf w Bash](https://www.tutorialspoint.com/unix_command/printf.htm)