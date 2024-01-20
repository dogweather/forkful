---
title:                "Praca z formatem json"
html_title:           "C++: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Co to jest JSON i dlaczego programiści z nim pracują?
JSON jest to format odwzorowujący dane w formacie tekstowym i jednocześnie używany jako środek przekazu informacji pomiędzy aplikacjami. Programiści wykorzystują go ze względu na jego prostotę, rozbudowane wsparcie w wielu językach programowania oraz szerokie zastosowanie w różnego rodzaju projektach.

## Jak to zrobić?
Aby pracować z JSON w C++, potrzebujemy biblioteki, która zaimplementuje odpowiednie funkcje do parsowania i generowania danych w tym formacie. Przykładowa biblioteka to [RapidJSON](http://rapidjson.org/), która jest szybka i lekka. Poniżej znajdują się przykłady kodu prezentujące, jak można korzystać z funkcji tej biblioteki.

```C++
// importujemy bibliotekę
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include <iostream>

// tworzymy przykładowy obiekt JSON
const char* json = "{\"kolor\":\"czerwony\",\"cena\":15.99,\"liczba_sztuk\":10}";

// parsujemy obiekt JSON
rapidjson::Document doc;
doc.Parse(json);

// wypisujemy wartości
std::cout << "Kolor: " << doc["kolor"].GetString() << std::endl;
std::cout << "Cena: " << doc["cena"].GetDouble() << std::endl;
std::cout << "Liczba sztuk: " << doc["liczba_sztuk"].GetInt() << std::endl;

// generujemy nowy obiekt JSON i wypisujemy go
rapidjson::Document doc2;
doc2.SetObject();
rapidjson::Document::AllocatorType& allocator = doc2.GetAllocator();
doc2.AddMember("imie", "Anna", allocator);
doc2.AddMember("wiek", 28, allocator);
doc2.AddMember("miasto", "Warszawa", allocator);
rapidjson::StringBuffer buffer;
rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
doc2.Accept(writer);
std::cout << buffer.GetString() << std::endl;
```

Powyższe przykłady pokazują sposób pracy z JSON przy użyciu biblioteki RapidJSON. W pierwszym przykładzie parsujemy istniejący obiekt JSON, a w drugim generujemy nowy obiekt i wypisujemy go w postaci tekstowej.

## Zagłębienie się w JSON
Format JSON został wynaleziony w 2001 roku przez Douga Crockforda, a jego popularność wzrosła wraz z rozwojem aplikacji internetowych. Obecnie jest często wykorzystywany w aplikacjach mobilnych, tworzeniu API i wymianie danych pomiędzy serwerem a klientem. Alternatywami dla JSON są między innymi formaty XML czy YAML, jednak JSON wyróżnia się swoją prostotą i szybkością działania.

W celu lepszego zrozumienia struktury i sposobu działania JSON można zapoznać się z jego specyfikacją na [oficjalnej stronie](https://www.json.org/json-pl.html). Istnieje również wiele dodatkowych bibliotek i narzędzi, które mogą ułatwić pracę z JSON, na przykład [JSON for Modern C++](https://github.com/nlohmann/json) czy [JSON Toolkit](https://github.com/cinemast/libjson).

## Zobacz również
- [Oficjalna strona JSON](https://www.json.org/json-pl.html)
- [RapidJSON - biblioteka do pracy z JSON w C++](http://rapidjson.org/)
- [JSON for Modern C++](https://github.com/nlohmann/json)