---
title:    "Arduino: Pisanie do standardowego błędu"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Arduino, na pewno słyszałeś o funkcji "Serial.print ()". Jest to bardzo przydatna funkcja, która pozwala na wypisywanie informacji na standardowe wyjście (Serial Monitor) w celu monitorowania działania programu. Ale czy wiedziałeś, że istnieje także funkcja "Serial.printErr ()", która pozwala na wypisywanie informacji na standardowe wyjście błędów (standard error)? W tym artykule dowiesz się dlaczego warto korzystać z funkcji "Serial.printErr ()" i jak to zrobić.

## Jak To Zrobić

Pierwszym krokiem jest otwarcie okna Serial Monitor w środowisku Arduino IDE, klikając na ikonę lupy w prawym górnym rogu lub wybierając opcję "Narzędzia" i "Serial Monitor". Następnie, w sekcji "Prędkość", wybierz odpowiednią szybkość transmisji - zalecamy 115200.

Aby użyć funkcji "Serial.printErr ()", musisz wcześniej zainicjować obiekt Serial w swoim kodzie. W tym celu wystarczy dodać linię "Serial.begin ()" w funkcji setup ().

Teraz możesz użyć funkcji "Serial.printErr ()" do wypisywania informacji na standardowe wyjście błędów. Na przykład, jeśli chciałbyś wypisać informację o błędzie w kodzie, możesz użyć następującej linijki:

```Arduino
Serial.printErr("Błąd! Nie można otworzyć pliku.");
```

Pamiętaj, że standardowe wyjście błędów jest różne od standardowego wyjścia, więc musisz użyć innej funkcji, żeby zobaczyć te informacje w Serial Monitorze. W sekcji "Linia kończąca" Serial Monitora wybierz opcję "Pojedyncza nowa linia" lub "Both NL & CR". W przeciwnym razie nie zobaczysz żadnych informacji w Serial Monitorze.

## Deep Dive

Standardowe wyjście błędów jest przydatne w wielu sytuacjach. Możesz go użyć do monitorowania błędów w kodzie, do debugowania lub nawet do wypisywania informacji o wyjątkach. Dodatkowo, w przeciwieństwie do standardowego wyjścia, standardowe wyjście błędów nie jest buforowane, więc informacje są wypisywane od razu.

## Zobacz Również

Jeśli jesteś zainteresowany bardziej szczegółowym wyjaśnieniem funkcji "Serial.printErr ()" oraz różnicami między standardowym wyjściem a standardowym wyjściem błędów, polecamy przeczytać następujące artykuły:

- ["Arduino - Serial.printErr ()" od hackster.io](https://www.hackster.io/MaxLeo961/arduino-serial-printerr-9d5bda)
- ["Serial.print () i Serial.printErr () - różnice" od programuj1.pl](https://programuj1.pl/arduino/o-seriale/serial-print-i-serial-printf-inna-jak/)

Dzięki funkcji "Serial.printErr ()" będziesz miał jeszcze większą kontrolę nad swoim programem i łatwiej będziesz mógł znaleźć i naprawić błędy. Nie wahaj się korzystać z tego narzędzia podczas tworzenia swoich projektów z Arduino!