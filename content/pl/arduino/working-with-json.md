---
title:                "Arduino: Praca z json"
simple_title:         "Praca z json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie Arduino może być fascynującym i satysfakcjonującym zajęciem dla każdego, kto interesuje się elektroniką i robotyką. Jednak niektóre projekty wymagają komunikacji z zewnętrznymi urządzeniami, takimi jak czujniki, wyświetlacze czy inne mikrokontrolery. W takich przypadkach bardzo przydatne jest wykorzystanie formatu JSON.

## Jak to zrobić

Arduino ma wbudowaną bibliotekę ArduinoJson, która umożliwia łatwe zarządzanie danymi w formacie JSON. Aby zacząć pracę z tą biblioteką, należy wykonać kilka kroków:

1. Zaimportuj bibliotekę do swojego projektu poprzez wybranie pozycji "Sketch" w menu głównym i wybierając "Include Library" > "ArduinoJson".
2. Zadeklaruj obiekt typu JSON w swoim kodzie: 
```Arduino
StaticJsonDocument<200> doc;
```
3. Przeczytaj przykładową wiadomość JSON z portu szeregowego:
```Arduino
Serial.setTimeout(50);
char json[] = "{\"sensor\": \"temperature\", \"value\": 25.5}";
deserializeJson(doc, Serial);
```
4. Teraz możesz łatwo uzyskać dostęp do danych JSON, np. wypisując wartość z pola "sensor" za pomocą komendy:
```Arduino
Serial.println(doc["sensor"].as<char*>());
```

Możesz również tworzyć i przetwarzać bardziej skomplikowane obiekty JSON, używając funkcji takich jak "createNestedObject" lub "prettyPrintTo". Aby uzyskać pełną listę możliwości i przykładów użycia, polecam zapoznanie się z oficjalną dokumentacją biblioteki ArduinoJson.

## Głębszy zanurzenie

JSON (JavaScript Object Notation) jest lekkim formatem wymiany danych, który jest szeroko wykorzystywany w aplikacjach internetowych. Jego głównym założeniem jest ułatwienie przetwarzania danych przez komputery i jednocześnie zachowanie czytelności dla ludzi. Dzięki prostocie struktury, JSON jest bardzo popularny i znajduje zastosowanie nie tylko w programowaniu, ale również w bazach danych czy w analizie big data.

Biblioteka ArduinoJson jest wysoce wydajna, co jest ważne w przypadku mikrokontrolerów takich jak Arduino, gdzie zasoby są ograniczone. Dodatkowo, podczas odczytu i zapisu danych JSON, można wybrać jedną z trzech dostępnych metod, w zależności od wymaganej wydajności i pamięci.

## Zobacz również

1. Oficjalna dokumentacja biblioteki ArduinoJson: https://arduinojson.org/v6/api/
2. Tutoriale oraz przykłady użycia biblioteki: https://arduinojson.org/v6/examples/
3. Szczegółowy artykuł o formacie JSON: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON