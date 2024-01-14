---
title:    "Bash: Pisanie do standardowego błędu"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, coraz więcej osób decyduje się na naukę programowania. Jednym z bardzo ważnych elementów znajdujących się w językach programowania jest standardowe wyjście błędu (ang. standard error). W tym artykule dowiesz się, dlaczego warto poznać i umiejętnie korzystać z tego elementu w języku Bash.

## Jak to zrobić

Aby napisać do standardowego wyjścia błędu, wystarczy użyć polecenia "echo" oraz przekierować wyjście do strumienia błędów (ang. stderr). Przykład:

```Bash
echo "Ten tekst pojawi się na standardowym wyjściu błędu" >&2
```

Output:

```sh
Ten tekst pojawi się na standardowym wyjściu błędu
```

W powyższym przykładzie widzimy, że używając przekierowania ">&2", nasz tekst pojawi się na standardowym wyjściu błędu zamiast na standardowym wyjściu (ang. stdout).

## Dogłębna analiza

W języku Bash standardowe wyjście błędu (stderr) jest odrębnym strumieniem danych, który jest wykorzystywany do wypisywania informacji o błędach w czasie wykonania programu. To bardzo ważne narzędzie podczas debugowania skryptów i aplikacji, ponieważ umożliwia nam wychwycenie błędów i szybką interwencję.

W przypadku, gdy nie przekierujemy wyjścia błędu do strumienia stderr, informacje o błędach będą wyświetlane na standardowym wyjściu, co może utrudnić nam zlokalizowanie błędu w naszym kodzie.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o standardowym wyjściu błędu w Bash, zapoznaj się z poniższymi artykułami:

- [Bash: Przekierowanie wejścia i wyjścia](https://linux.die.net/Bash-Beginners-Guide/moreadvanced.html)
- [Przekierowanie strumieni w Bash](https://www.tutorialspoint.com/unix/unix-i-o-redirections.htm)