---
title:    "Arduino: Wydrukuj wyjście debugowania"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Podczas programowania projektów z wykorzystaniem Arduino, często spotykamy się z koniecznością debugowania kodu. W tym artykule dowiesz się, dlaczego wyświetlanie outputu debugowania jest ważne i jak możesz to zrobić.

## Jak to zrobić

Arduino oferuje nam wiele sposobów na wyświetlanie informacji debugujących. Jednym z najpopularniejszych i najprostszych sposobów jest użycie funkcji `Serial.print()`. Pozwala ona na wyświetlenie tekstu lub wartości zmiennych w monitorze szeregowym. Przykład użycia:

```Arduino
int liczba = 5; // Tworzymy zmienną o wartości 5
Serial.begin(9600); // Inicjalizujemy port szeregowy z prędkością 9600 bps
Serial.print("Liczba: "); // Wyświetlamy tekst "Liczba: " w terminalu
Serial.println(liczba); // Wyświetlamy wartość zmiennej "liczba" w terminalu, z automatycznym dodaniem nowej linii
```

Wynik w monitorze szeregowym powinien wyglądać tak:

```
Liczba: 5
```

Możemy również wyświetlać więcej informacji za pomocą funkcji `Serial.println()` poprzez połączenie wielu wyrażeń. Przykład:

```Arduino
int liczba1 = 10;
int liczba2 = 20;
Serial.println("Wynik dodawania: " + String(liczba1 + liczba2));
```

Wynik w monitorze szeregowym:

```
Wynik dodawania: 30
```

Inną metodą jest wykorzystanie biblioteki `Wire.h` do komunikacji przez interfejs I2C. Dzięki temu możemy przesyłać informacje z Arduino do innych urządzeń, np. modułu wyświetlacza LCD.

## Deep Dive

Wyświetlanie debug outputu ma dużo większe znaczenie, niż tylko wyświetlenie zawartości zmiennych w monitorze szeregowym. Dzięki temu możemy śledzić przebieg naszego kodu, wykryć błędy i poprawić działanie projektu. Jest to niezwykle przydatne w przypadku bardziej zaawansowanych projektów, gdzie jedna niepoprawna linijka kodu może powodować awarię całego systemu.

Podczas debugowania ważne jest również wykorzystywanie różnych poziomów debug outputu, aby nie zasypanym się zbyt dużą ilością informacji. Jednym ze sposobów na to jest wykorzystanie warunkowych wyrażeń, które wyświetlą informacje tylko w przypadku spełnienia pewnego warunku. Przykład:

```Arduino
int temperatura = analogRead(A0); // Odczytujemy wartość z czujnika temperatury
if (temperatura > 25) // Jeśli temperatura jest większa od 25 stopni
{
  Serial.println("Temperatura za wysoka!"); // Wyświetlamy komunikat
}
```

W takim przypadku, jeśli temperatura będzie poniżej 25 stopni, nie pojawi się żaden output w monitorze szeregowym.

## Zobacz też

- [Jak debugować kod w Arduino](https://www.arduino.cc/en/Guide/ArduinoDebugger)
- [Wyświetlanie outputu debugowania w Arduino](https://forum.arduino.cc/index.php?topic=119995.0)
- [Biblioteka Wire.h](https://www.arduino.cc/en/reference/wire)