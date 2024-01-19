---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Analiza składniowa HTML polega na czytaniu i analizowaniu kodu HTML w celu zrozumienia jego struktury i zawartości. Programiści robią to, aby wykorzystać, wydobyć lub manipulować danymi zawartymi w stronach internetowych.

## Jak to zrobić:

Teraz zrozumiesz podstawy analizy składni HTML. Poniżej podaję przykładowy kod z użyciem Arduino wraz z wyjściem.

```Arduino
#include <Ethernet.h>
#include <HTMLParser.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192,168,1,1);
EthernetClient client;
HTMLParser htmlParser;

void setup() {
    Serial.begin(9600); 
    Ethernet.begin(mac, ip); 
}

void loop() {
    if (client.connect("www.example.com", 80)) {
     client.println("GET / HTTP/1.1");
     client.println("Host: www.example.com");
     client.println("Connection: close");
     client.println();
     htmlParser.begin();
     while(client.connected() && !client.available()); 
     while (client.available()){
        htmlParser.processInput((char)client.read());
     }
    client.stop();
    }
}
```

Tutaj skrypt łączy się z www.example.com i przeprowadza analize składni HTML strony głównej.

## Głębsze zanurzenie

Analiza składniowa HTML ma swoje korzenie w początkach tworzenia sieci, kiedy strony internetowe były tworzone i przeglądane w postaci czystego kodu HTML. Uzyskanie dostępu do treści strony wymagało "rozumienia" kodu HTML, stąd konieczność analizy składniowej. Istnieją alternatywy dla analizy składniowej HTML, takie jak wykorzystanie API, które zwraca dane w bardziej przyswajalnym formacie, takim jak JSON. Jednakże, nie wszystkie strony oferują API i czasami analiza składniowa HTML jest jedynym sposobem na dostęp do danych.

## Zobacz też

- Dokumentacja Arduino: https://www.arduino.cc/reference/en/
- Instrukcje dla innych procesorów HTML: https://htmlparser.sourceforge.io/
- Przykładowe projekty Arduino: https://create.arduino.cc/projecthub

Produktywnego kodowania! Pomocne mogą być te dodatkowe źródła.