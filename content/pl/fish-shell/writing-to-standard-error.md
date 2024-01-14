---
title:    "Fish Shell: Pisanie do standardowego błędu"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu (standard error) jest ważnym narzędziem w programowaniu w Fish Shell. Pomimo tego, że może brzmieć nieco skomplikowanie, pozwala ono na wyświetlanie komunikatów o błędach lub ostrzeżeniach, co jest niezbędne do prawidłowego debugowania kodu.

## Jak to zrobić

Jest kilka sposobów na napisanie do standardowego błędu w Fish Shell. Jednym z nich jest użycie polecenia "fprintf", które działa podobnie jak w innych językach programowania. Przykładowe użycie wyglądałoby następująco:

```
Fish Shell ...
fprintf stderr "To jest komunikat błędu!"
```

W powyższym kodzie, "fprintf" określa, do którego strumienia ma zostać wysłana wiadomość, a później podajemy sam komunikat, który ma zostać wyświetlony. Należy również pamiętać o użyciu "2>" przed komendami, aby przekierować je do standardowego błędu.

## Deep Dive

Głębsze zanurzenie w temacie pisania do standardowego błędu wymaga zrozumienia różnic między standardowym wejściem (standard input), standardowym wyjściem (standard output) oraz standardowym błędem (standard error). Standardowe wejście służy do przekazania danych do programu, standardowe wyjście do wypisywania wyników, a standardowy błąd do komunikatów o błędach i ostrzeżeń.

Pisanie do standardowego błędu jest przydatne w przypadkach, gdy chcemy odróżnić informacje o błędach od standardowego wyjścia. Możemy również przekierować standardowy błąd do pliku, aby odebrać komunikaty o błędach w łatwiejszy sposób.

## Zobacz także

- Wprowadzenie do standardowych strumieni w Fish Shell: [link](https://fishshell.com/docs/current/tutorial.html#tut_streams)
- Dokumentacja Fish Shell na temat strumieni: [link](https://fishshell.com/docs/current/index.html#stderr)
- Poradnik o przekierowywaniu strumieni: [link](https://www.howtogeek.com/435903/what-are-stdin-stdout-and-stderr-on-linux/)