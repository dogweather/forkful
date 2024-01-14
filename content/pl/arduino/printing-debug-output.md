---
title:    "Arduino: Drukowanie informacji diagnostycznych"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, jako programista Arduino, potrzebujemy dodatkowej informacji na temat działania naszego kodu. Drukowanie danych debuggowania jest jednym ze sposobów, aby uzyskać wgląd w to, co dzieje się w trakcie wykonywania programu. Może to pomóc nam w identyfikacji błędów lub zapewnieniu, że nasza logika jest poprawna.

## Jak to zrobić

Aby wyświetlić dane debugowania w Arduino, możemy użyć funkcji `Serial.print()` lub `Serial.println()`. Poniższy przykład pokazuje, jak wyświetlić wartość zmiennej `x`:

```Arduino
int x = 5;
Serial.print("Wartość x to: ");
Serial.println(x);
```

Wyjście powyższego kodu będzie wyglądać następująco:

```
Wartość x to: 5
```

Możemy również użyć `Serial.print()` do wyświetlenia tekstu bezpośrednio, na przykład:

```Arduino
Serial.print("To jest tekst bez zmiennej.");
```

## Głębsza wiedza

Funkcje `Serial.print()` i `Serial.println()` nie są jedynymi sposobami na wyświetlanie danych debugowania. Istnieją również inne funkcje, które mogą nam pomóc w analizie naszego kodu, takie jak `Serial.write()` lub `Serial.printf()`. Warto zapoznać się z dokumentacją Arduino, aby poznać wszystkie dostępne opcje.

Pamiętaj, aby nie pozostawiać danych debugowania w swoim kodzie, gdy już skończysz pracę nad projektem. Zbyt dużo wyświetlanych informacji może wpłynąć na wydajność programu i zajmować zbędne miejsce w pamięci Arduino.

## Zobacz również

- [Dokumentacja Arduino o drukowaniu danych debugowania](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Inne sposoby na debugowanie kodu Arduino](https://www.makerguides.com/debug-arduino-code/)
- [10 porad dotyczących debugowania Arduino](https://www.arduino.cc/en/Guide/Troubleshooting)