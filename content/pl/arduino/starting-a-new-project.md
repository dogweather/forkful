---
title:                "Arduino: Rozpoczynanie nowego projektu"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Zacząć nowy projekt z Arduino może być ekscytującym wyzwaniem dla każdego, kto lubi programowanie i elektronikę. Dzięki Arduino możesz stworzyć różnorodne urządzenia, od prostych czujników po skomplikowane roboty.

## Jak To Zrobić

Aby rozpocząć projekt z Arduino, musisz najpierw pobrać i zainstalować środowisko Arduino IDE na swoim komputerze. Następnie wybierz odpowiedni model płytki Arduino dla swojego projektu i podłącz ją do komputera za pomocą kabla USB.

Następnie możesz przejść do pisania kodu w języku Arduino, który jest oparty na języku C++. Wprowadź swoje poniższyhino Arduino używając odpowiedniej składni:

```Arduino
void setup() {
  // kod inicjalizujący, wykonuje się tylko raz
}

void loop() {
  // kod, który będzie wykonywany w pętli
}
```

W sekcji "setup" umieść kod, który jest potrzebny do inicjalizacji projektu, taki jak ustawienie pinów wejściowych/wyjściowych czy połączenie z modułem WiFi. W sekcji "loop" umieść kod, który ma być wykonywany w pętli przez cały czas działania projektu.

Możesz też używać różnorodnych funkcji i bibliotek, aby rozszerzyć funkcjonalność swojego projektu. Na przykład, jeśli potrzebujesz obsługi czujnika temperatury, możesz skorzystać z biblioteki "OneWire" i "DallasTemperature". Poniżej znajduje się przykładowy kod, który wykorzystuje te biblioteki do odczytu temperatury z czujnika DS18B20:

```Arduino
#include <OneWire.h>
#include <DallasTemperature.h>

// ustawienie pinu dla czujnika
#define ONE_WIRE_BUS 2

// inicjalizacja obiektów OneWire i DallasTemperature
OneWire oneWire(ONE_WIRE_BUS);
DallasTemperature sensors(&oneWire);

// zmienna, do której zostanie zapisana odczytana temperatura
float temperature = 0;

void setup() {
  // inicjalizacja komunikacji i czujnika
  sensors.begin();
}

void loop() {
  // odczytanie temperatury i zapisanie wartości do zmiennej
  sensors.requestTemperatures();
  temperature = sensors.getTempCByIndex(0);
  
  // wyświetlenie odczytanej temperatury na monitorze szeregowym
  Serial.println(temperature);
  delay(1000); // opóźnienie w ms
}
```

## Głębsze Wnioskowanie

Aby zacząć nowy projekt z Arduino, musisz mieć pomysł i wiedzę na temat programowania oraz elektroniki. Jeśli jesteś nowicjuszem w tym temacie, warto przejrzeć dostępne na internecie tutoriale i poradniki, aby zrozumieć podstawowe zagadnienia. Wiele błędów w kodzie można uniknąć poprzez dokładne zapoznanie się z dokumentacją i przykładami, dostępnymi na oficjalnej stronie Arduino oraz innych zasobach online.

Niektóre projekty z Arduino mogą wymagać również użycia płytki rozszerzeń, takiej jak shield, aby zwiększyć ilość dostępnych pinów i dodatkowych funkcji. Ważne jest, aby wybrać odpowiednią płytkę dla swojego projektu, aby wszystko działało bez problemów.

## Zobacz Również

- Oficjalna strona Arduino (https://www.arduino.cc/)
- Dokumentacja Arduino (https://www.arduino.cc/reference/en/)
- Instrukcje i poradniki (https://create.arduino.cc/projecthub)
- Kursy i szkolenia online (https://www.coursera.org/courses?query=arduino)