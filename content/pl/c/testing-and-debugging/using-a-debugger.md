---
title:                "Korzystanie z debugera"
aliases:
- /pl/c/using-a-debugger.md
date:                  2024-02-03T18:09:59.651462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Debuggery w języku C to specjalistyczne narzędzia, które pozwalają programistom krok po kroku przeglądać swój kod, inspekcjonować zmienne i monitorować przepływ wykonania. Proces ten jest niezbędny do identyfikowania i naprawiania błędów, zapewniając, że kod zachowuje się zgodnie z oczekiwaniami.

## Jak to zrobić:

GDB (GNU Debugger) jest najczęściej używanym debugerem dla programowania w języku C. Oto krótki przewodnik, jak używać GDB do debugowania prostego programu w C.

Najpierw skompiluj swój program w C z flagą `-g`, aby dołączyć informacje debugowania:

```c
gcc -g program.c -o program
```

Następnie uruchom GDB ze skompilowanym programem:

```bash
gdb ./program
```

Teraz możesz użyć różnych poleceń w GDB, aby kontrolować jego działanie. Oto kilka podstawowych poleceń:

- `break`: Ustaw punkt przerwania w określonej linii lub funkcji, aby zatrzymać wykonanie.
  - Przykład: `break 10` lub `break main`
- `run`: Rozpocznij wykonanie programu w GDB.
- `next`: Wykonaj następną linię kodu, nie wchodząc do funkcji.
- `step`: Wykonaj następną linię kodu, wchodząc do funkcji.
- `print`: Wyświetl wartość zmiennej.
- `continue`: Wznów wykonanie do następnego punktu przerwania.
- `quit`: Wyjdź z GDB.

Oto przykład sesji debugowania prostego programu:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Skompiluj i uruchom GDB, jak opisano. Ustaw punkt przerwania przy linii `printf` używając `break 5`, a następnie `run`. Użyj `next`, aby krok po kroku przejść przez pętlę i `print i`, aby zbadać zmienną pętli.

Przykładowy wynik po ustawieniu punktu przerwania i przed pierwszą iteracją:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Używanie `print i` po kilku iteracjach:

```
$3 = 2
```

To demonstruje badanie stanu i przepływu prostego programu.

## Wgłębianie się

Koncepcja debugowania znacznie ewoluowała od wczesnych dni programowania, gdzie fizyczne błędy (dosłowne owady) mogły powodować problemy w mechanicznych komputerach. Dzisiaj debuggery takie jak GDB oferują zaawansowane funkcje wykraczające poza podstawowe kroki i inspekcje zmiennych, takie jak debugowanie wsteczne (wykonywanie programu do tyłu), warunkowe punkty przerwania i skrypty do automatycznych zadań debugowania.

Chociaż GDB jest potężny i szeroko używany, może być gęsty i trudny dla początkujących. Alternatywne narzędzia do debugowania i środowiska zintegrowane (IDE) takie jak Visual Studio Code, CLion lub Eclipse oferują bardziej przyjazne dla użytkownika interfejsy do debugowania kodu C, często integrując wizualne pomoce i bardziej intuicyjne kontrole. Te alternatywy mogą nie oferować pełnej głębi funkcjonalności GDB, ale mogą być bardziej dostępne dla nowicjuszy w programowaniu w języku C.

Ponadto, pojawienie się protokołów serwera językowego i standardów debugowania ułatwiło rozwiązania do debugowania międzyplatformowego, czyniąc doświadczenie debugowania bardziej spójnym w różnych narzędziach i środowiskach. Pomimo tych postępów, nauka detali tradycyjnego debugera jak GDB dostarcza cennych wglądów w wykonanie programów w języku C i pozostaje kluczową umiejętnością w zestawie narzędzi programisty.
