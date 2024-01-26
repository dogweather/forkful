---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T00:59:29.046283-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
"Logowanie" to tworzenie zapisu wydarzeń, transakcji lub działań, które mają miejsce w czasie w systemie. Programiści używają go do debugowania, monitorowania stanu systemu, zbierania statystyk, a nawet audytu używania, co czyni je nieodzowną praktyką dla utrzymania i zrozumienia zachowania ich kodu w różnych warunkach.

## Jak to zrobić:
Arduino nie posiada wbudowanej biblioteki do logowania, tak jak niektóre inne środowiska, ale można zaimplementować podstawowe logowanie do konsoli szeregowej bez większego problemu. Oto krótki przykład, który pomoże Ci zacząć:

```arduino
void setup() {
  // Rozpocznij komunikację szeregową z podaną szybkością transmisji
  Serial.begin(9600);

  // Czekaj na połączenie z portem szeregowym - tylko konieczne w niektórych płytach
  while (!Serial) {
    ; // czekaj na połączenie z portem szeregowym. Konieczne dla natywnego USB
  }

  // Zaloguj informacyjną wiadomość wskazującą, że proces konfiguracji jest zakończony
  Serial.println("Konfiguracja zakończona!");
}

void loop() {
  // Prosty logger, który wyświetla czas pracy co sekundę
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Czas pracy (ms): ");
    Serial.println(currentMillis);

    // Tutaj możesz również dodać logi błędów, ostrzeżenia lub inne informacje.
  }
  
  // Reszta logiki programu tutaj...
}
```

Przykładowe wyjście Serialne:
```
Konfiguracja zakończona!
Czas pracy (ms): 1000
Czas pracy (ms): 2000
Czas pracy (ms): 3000
...
```

## Szczegółowa analiza:
Historycznie rzecz biorąc, logowanie na mikrokontrolerach nie było tak proste jak w pełnoprawnym systemie operacyjnym. Ograniczone zasoby oznaczały, że każdy bajt się liczył i programiści musieli uważać, aby nie zatkać systemu. Z rozwojem bardziej zaawansowanych płyt i platformy Arduino ułatwiającej proces, logowanie stało się bardziej dostępne.

Chociaż powyższy kod demonstrowany jest poprzez interfejs szeregowy, inne metody obejmują zapis na kartę SD, wysyłanie danych poprzez sieć na zdalny serwer, czy nawet wyświetlanie na małym wyświetlaczu LCD.

Implementacja systemu logowania wiąże się z rozważeniami, takimi jak rotacja, poziom ważności (info, debug, ostrzeżenie, błąd) oraz wpływ na wydajność. Na Arduino może być konieczna ostrożność dotycząca ograniczeń pamięciowych przy logowaniu skomplikowanych struktur danych. W przypadku zdalnego logowania, ważne jest również zabezpieczenie przesyłanych logów.

Istnieją bardziej zaawansowane rozwiązania, takie jak Syslog, szeroko przyjęty standard logowania, który istnieje poza światem Arduino, ale można zintegrować biblioteki stron trzecich, które oferują podobną funkcjonalność z różnymi stopniami złożoności i wymagań dotyczących zasobów.

## Zobacz również:
- [Referencja `Serial` Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Logowanie na karcie SD z Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [Tarcza do logowania danych SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb: Praktyczny przykład zdalnego logowania z Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)