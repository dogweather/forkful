---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Arduino: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest niezbędnym elementem programowania mikrokontrolerów, takich jak Arduino. Pozwala ono na tymczasowe przechowywanie danych, które nie są potrzebne przez cały czas działania urządzenia. Jest to szczególnie przydatne w przypadku obsługi sensorów lub zapisywania tymczasowych wyników obliczeń.

## Jak to zrobić

Tworzenie plików tymczasowych w Arduino jest niezwykle proste. Wystarczy użyć funkcji ```File::createTemp()``` i przekazać jako argument nazwę pliku. Na przykład:

```Arduino
File tempFile = File::createTemp("moj_plik");
```

W ten sposób zostanie utworzony plik tymczasowy o nazwie "moj_plik". Możemy go później wykorzystać do przechowywania danych i odczytywać lub zapisywać do niego za pomocą standardowych funkcji Arduino dla operacji na plikach.

## Deep Dive

Głębiej zagłębiając się w temat, warto wspomnieć o sposobie działania funkcji ```File::createTemp()```. Tworzy ona plik w pamięci operacyjnej mikrokontrolera, co oznacza, że istnieje on tylko do momentu, gdy urządzenie jest włączone. Po wyłączeniu i ponownym uruchomieniu plik ten zostanie usunięty. Jest to więc idealne rozwiązanie dla danych, które są potrzebne tylko przez krótki okres czasu.

Również warto dodać, że funkcja ta jest dostępna tylko w niektórych wersjach Arduino, takich jak Arduino Due, Zero czy M0. W innych wersjach konieczne może być wykorzystanie zewnętrznego modułu pamięci dla przechowywania plików tymczasowych.

## Zobacz również

- Dokumentacja funkcji ```File::createTemp()```: [link](https://www.arduino.cc/reference/en/libraries/file-system-library/file/createtemp/)
- Przykłady wykorzystania plików tymczasowych w Arduino: [link](https://create.arduino.cc/projecthub/drederek/create-a-temporary-log-file-on-sd-card-arduino-uno-sd-spi-lib-server-3b268e)
- Poradnik na temat działania pamięci operacyjnej w mikrokontrolerach: [link](https://www.allaboutcircuits.com/technical-articles/working-with-random-access-memory/)