---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?
YAML to format danych używany przez programistów do przechowywania konfiguracji aplikacji. Daje możliwość zapisania danych w czytelnej dla człowieka formie, co ułatwia edycję oraz utrzymanie. Programiści wybierają YAML zamiast tradycyjnych form ustawień, takich jak pliki .json lub .xml, ponieważ jest on prostszy w użyciu i ma ładniejszą składnię.

## Jak to zrobić:
Korzystanie z YAML w Arduino jest proste. Możesz użyć biblioteki do obsługi tego formatu danych lub napisać własną funkcję do parsowania plików YAML. Poniżej przedstawiono przykładowy kod, który odczytuje dane z pliku YAML i wyświetla je na ekranie:
```Arduino
// pobieranie danych z pliku
String daneYAML = readYAML("ustawienia.yaml");
// wyświetlanie danych na ekranie
Serial.println(daneYAML);
```

## Mocne i słabe strony:
YAML został stworzony w 2001 roku przez Clarka Evansa, Ingy döt Net oraz Oren Ben-Kiki. Jest on powszechnie używany w różnych językach programowania, takich jak Java, Python czy Ruby. Jedną z jego głównych zalet jest czytelność dla człowieka. Jednak niektórzy programiści uważają, że jest on mniej wydajny niż inne formaty. Pamiętaj, aby odpowiednio skalować wykorzystanie YAML w swoich projektach.

## Zobacz także:
Jeśli chcesz dowiedzieć się więcej o pracy z YAML w Arduino, zapoznaj się z oficjalną dokumentacją: https://www.arduino.cc/reference/en/libraries/yaml/. Możesz także poszukać przykładowych projektów w sieci lub na forach dla programistów.