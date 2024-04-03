---
date: 2024-01-26 04:27:47.056171-07:00
description: "Praca z XML na Arduino polega na analizowaniu (parsowaniu) i manipulowaniu\
  \ danymi XML, kt\xF3re zwykle pochodz\u0105 z interfejs\xF3w API sieci Web lub plik\xF3\
  w\u2026"
lastmod: '2024-03-13T22:44:35.694931-06:00'
model: gpt-4-0125-preview
summary: "Praca z XML na Arduino polega na analizowaniu (parsowaniu) i manipulowaniu\
  \ danymi XML, kt\xF3re zwykle pochodz\u0105 z interfejs\xF3w API sieci Web lub plik\xF3\
  w konfiguracyjnych."
title: Praca z XML
weight: 40
---

## Co i dlaczego?
Praca z XML na Arduino polega na analizowaniu (parsowaniu) i manipulowaniu danymi XML, które zwykle pochodzą z interfejsów API sieci Web lub plików konfiguracyjnych. Programiści robią to, aby zintegrować się z usługami, które używają XML do wymiany danych lub aby przechowywać dane w strukturyzowanym, czytelnym dla człowieka formacie.

## Jak to zrobić:
Użyjemy biblioteki `XMLWriter` do tworzenia dokumentów XML oraz biblioteki `tinyxml2` do ich analizy. Najpierw zainstaluj biblioteki za pomocą menedżera bibliotek w Twoim środowisku IDE Arduino.

Tworzenie dokumentu XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Używając Serial do wyjścia
  
  xml.header();
  xml.tag("greeting").tag("text").text("Witaj, świecie!").close().close();
  xml.flush();
}

void loop() {
}
```

Dekodowanie ciągu XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Witaj, świecie!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Przykładowe wyjście:

```
<greeting>
  <text>Witaj, świecie!</text>
</greeting>
```

## Wgłębianie się
XML, czyli rozszerzalny język znaczników, to język znaczników definiujący zestaw zasad do kodowania dokumentów w formacie, który jest czytelny zarówno dla człowieka, jak i maszyny. XML istnieje od końca lat 90. i jest szeroko stosowany w różnych dziedzinach, zwłaszcza tam, gdzie potrzebna jest wymiana danych niezależna od platformy. Ograniczone zasoby pamięci Arduino sprawiają, że praca z XML jest bardziej wymagająca niż na komputerze. Dlatego kluczowe są lekkie biblioteki. Chociaż JSON zyskał na popularności do wymiany danych dzięki swojej prostszej składni i mniejszemu śladowi, XML jest nadal szeroko stosowany, szczególnie w przypadku systemów dziedziczonych lub aplikacji wymagających walidacji dokumentów za pomocą schematów. Kluczem do implementacji XML w Arduino jest analiza strumieniowa, która odczytuje dokument w segmentach, aby utrzymać niskie zużycie pamięci.

## Zobacz także
- [Dokumentacja biblioteki TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Biblioteka Arduino JSON](https://arduinojson.org/) jako alternatywa przy pracy z danymi JSON.
- [Samouczek XML z W3Schools](https://www.w3schools.com/xml/) do ogólnej nauki XML.
- [Specyfikacja XML W3C](https://www.w3.org/XML/) dla oficjalnych standardów i rekomendacji XML.
