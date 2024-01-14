---
title:                "Arduino: Praca z plikami YAML"
simple_title:         "Praca z plikami YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, że jedną z najważniejszych rzeczy w tworzeniu oprogramowania jest przechowywanie danych. Jednym ze sposobów na przechowywanie i organizowanie danych jest używanie YAML (Yet Another Markup Language). Dla programistów Arduino, znajomość YAML jest niezwykle przydatna, ponieważ pozwala na łatwiejszą pracę z różnymi formatami danych. W tym artykule pokażemy dlaczego warto uczyć się YAML oraz jak zacząć pracę z nim w środowisku Arduino.

## Jak To Zrobić

Aby używać YAML w Arduino, musisz najpierw zainstalować bibliotekę "YAM". Możesz to zrobić poprzez Arduino IDE lub ręcznie z pobranego pliku ZIP. Następnie, aby rozpocząć pracę z YAML, musisz zadeklarować jej używanie w kodzie:

```Arduino
#include <yaml.h>
```

Teraz możesz tworzyć obiekty YAML i zapisywać oraz odczytywać dane. Przykładowy kod poniżej demonstruje tworzenie obiektu YAML i zapisanie do niego danych:

```Arduino
YAML::Emitter yaml; // Tworzenie obiektu YAML
yaml << YAML::BeginMap; // Rozpoczęcie mapy
yaml << YAML::Key << "imie"; // Ustalenie klucza
yaml << YAML::Value << "Jan"; // Ustalenie wartości
yaml << YAML::Key << "wiek";
yaml << YAML::Value << 30;
yaml << YAML::EndMap; // Zakończenie mapy
Serial.println(yaml.c_str()); // Wypisanie danych do monitora szeregowego
```

Output dla powyższego kodu będzie wyglądał mniej więcej tak:

```
imie: Jan
wiek: 30
```

## Głębszy Zanurzenie

Powyższe przykłady są jedynie wprowadzeniem do pracy z YAML w Arduino. W rzeczywistości możesz przesyłać i odczytywać wiele różnych typów danych, takich jak liczby, tablice i obiekty. Możesz również używać YAML do przechowywania i ładowania stanów obwodu lub konfiguracji urządzenia. Nauka więcej o możliwościach YAML i jak je wykorzystać w środowisku Arduino może znacznie ułatwić tworzenie zaawansowanych projektów.

## Zobacz również

- Dokumentacja Arduino dla biblioteki YAML: https://www.arduino.cc/reference/en/libraries/yaml/
- Przewodnik po YAML dla początkujących: https://yaml.org/start.html
- Przykłady zastosowania YAML w Arduino: https://programmingelectronics.com/yaml-and-arduino/
- Oficjalna strona YAML: https://yaml.org/