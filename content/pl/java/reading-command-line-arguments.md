---
title:    "Java: Odczytywanie argumentów wiersza poleceń"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego warto poznać argumenty wiersza poleceń w programowaniu Java?

Jeśli jesteś programistą Java, zapewne dobrze znasz zastosowanie argumentów wiersza poleceń w uruchamianiu programów. Jednak czy wiesz, że można również odczytywać te argumenty wewnątrz samego programu? Poznajmy dlaczego jest to ważne i jak to zrobić.

## Jak to zrobić?

Aby odczytywać argumenty wiersza poleceń w programowaniu Java, musimy użyć obiektu `args` klasy `String[]`. Następnie, wewnątrz metody `main` możemy wykorzystać pętlę `for` do odczytania poszczególnych argumentów.

```Java
public static void main(String[] args) {
    // pętla for odczytująca argumenty wiersza poleceń
    for (int i = 0; i < args.length; i++) {
        System.out.println("Argument " + (i+1) + ": " + args[i]);
    }
}
```

Aby uruchomić ten kod, możemy przekazać argumenty wiersza poleceń w następujący sposób:

`java MyClass argument1 argument2 argument3`

W ten sposób, nasz program odczyta wszystkie trzy argumenty i wypisze je na ekranie.

Output:
```
Argument 1: argument1
Argument 2: argument2
Argument 3: argument3
```

## Wnikliwa analiza

Odczytywanie argumentów wiersza poleceń może być bardzo przydatne w różnych scenariuszach programistycznych. Na przykład, możemy wykorzystać je do sterowania działaniem programu lub do przekazywania danych z jednego programu do drugiego.

Jednym z ciekawszych zastosowań jest tworzenie programów, które mogą działać w trybie interaktywnym lub automatycznym, w zależności od tego czy zostaną przekazane argumenty wiersza poleceń. Jest to szczególnie przydatne przy tworzeniu testów czy skryptów.

Warto również wspomnieć, że oprócz odczytywania argumentów, mamy możliwość również przekazywania ich do programu. W tym celu używamy funkcji `System.setProperty()` i przypisujemy wartość argumentu do wybranego klucza.

## Zobacz również
- [Oracle: Przekazywanie argumentów wiersza poleceń](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial Java: Jak czytać argumenty wiersza poleceń?](https://javatutorial.net/java-command-line-argument-parsing)
- [Dokumentacja Java: System class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html)