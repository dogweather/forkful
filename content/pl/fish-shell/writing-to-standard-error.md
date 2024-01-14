---
title:    "Fish Shell: Pisanie do standardowego błędu"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu (standard error) jest często używaną techniką w programowaniu w języku Fish Shell. Jest to przydatne w celu przekazywania informacji o błędach lub ostrzeżeń w trakcie wykonywania skryptów.

## Jak

Aby napisać do standardowego błędu w języku Fish Shell, używamy polecenia `echo` wraz z przekierowaniem wyjścia do `stderr` za pomocą symbolu `>`.

```Fish Shell
echo "To jest błąd" > stderr
```

Jeśli chcemy przekazać więcej niż jedną linijkę tekstu, możemy użyć operatora `&` oraz wartości `endl`, co spowoduje przeniesienie kursora do nowej linii po każdej wypisanej linii.

```Fish Shell
echo "To jest ostrzeżenie" endl "Jest coś nie tak" > stderr
```

Rezultatem powyższych poleceń będzie wypisanie tekstu do standardowego błędu, który można następnie wykorzystać w dalszej części skryptu.

## Deep Dive

Pisanie do standardowego błędu jest przydatne, ponieważ pozwala nam na przekazywanie informacji o błędach lub ostrzeżeń w trakcie wykonywania skryptów. Jest to ważne, ponieważ w ten sposób możemy szybko zidentyfikować i rozwiązać potencjalne problemy w naszych skryptach.

Ponadto, pisanie do standardowego błędu jest często wykorzystywane w procesie debugowania kodu. Dzięki temu możemy śledzić wartości zmiennych lub wyjścia funkcji w trakcie wykonywania skryptu, co ułatwia znalezienie przyczyny błędu.

## Zobacz także

- [Oficjalna dokumentacja języka Fish Shell](https://fishshell.com/docs/current/index.html)
- [Dokumentacja polecenia `echo`](https://fishshell.com/docs/current/cmds/echo.html)
- [Poradnik dla początkujących w języku Fish Shell](https://dev.to/dahamstaft/fish-shell-a-beginner-s-guide-3l0l)