---
title:                "Drukowanie wyników debugowania"
html_title:           "Java: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# Czym i dlaczego?

W druku kodów debugowania chodzi o wyświetlanie informacji o przebiegu działania programu w celu znalezienia i naprawienia błędów. Programiści używają go, aby zrozumieć, co się dzieje w kodzie i gdzie mogą pojawić się problemy.

# Jak to zrobić:

Kodowania przykładów i wyjścia przykładów w blokach kodu ```Java ...``` 

Przykład 1:
```Java
System.out.println("Debug message: Program starting.");
```
Wyjście:
```
Debug message: Program starting.
```
Przykład 2:
```Java
int x = 5;
System.out.println("Debug message: x = " + x);
```
Wyjście:
```
Debug message: x = 5
```

# Głębsze zanurzenie:

Kodowanie debugowania ma swoje korzenie w przeszłości, kiedy programiści musieli przeszukiwać długie listy komend lub arkusze papieru, aby znaleźć błędy. Dzisiaj istnieje wiele alternatywnych metod, takich jak debugger, które pozwalają programistom na łatwiejsze znajdowanie błędów. Implementacja polega na wprowadzeniu funkcji wyświetlania informacji w kodzie i kompilowaniu go.

# Zobacz również:

Jeśli chcesz dowiedzieć się więcej o debuggingu i innych narzędziach dla programistów, polecamy przeczytać poniższe źródła:
- [10 przydatnych technik debugowania w Javie](https://pl.twinmeta.com/how-to-debug-java-application/)
- [Wyjaśnienie debugowania w Java z przykładami](https://przeprogramowani.pl/programowanie-wysokiego-poziomu/debugowanie-w-java/)
- [Oficjalne dokumentacje Oracle o debugowaniu w Javie](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)