---
title:                "Arduino: Odczytywanie html"
simple_title:         "Odczytywanie html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie Arduino jest wspaniałym sposobem na tworzenie interaktywnych projektów używając prostego języka programowania. Jednym z najbardziej przydatnych aspektów programowania Arduino jest możliwość parsowania (czytania) kodu HTML. Nie tylko pozwala to na wyświetlanie danych z Internetu, ale także otwiera możliwość tworzenia własnych interaktywnych stron internetowych bez potrzeby posiadania rozbudowanych umiejętności programistycznych.

## Jak to zrobić

Aby rozpocząć parsowanie HTML za pomocą Arduino, potrzebujemy jedynie kilku prostych narzędzi. Pierwszym z nich jest biblioteka HTMLParser, dostępna do pobrania ze strony github.com/sirleech/HTMLParser. Następnie, utwórz nowy projekt Arduino i zaimportuj bibliotekę HTMLParser do swojego projektu.

Teraz przyjrzyjmy się najważniejszej części kodu. W celu parsowania strony HTML, musimy najpierw wczytać dane ze strony internetowej. Można to zrobić za pomocą kodu:

```Arduino
#include <SPI.h>
#include <Ethernet.h>
#include <HTMLParser.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192,168,1,177);
IPAddress gateway(192,168,1,1);
IPAddress subnet(255,255,255,0);

EthernetClient client;

void setup() {
  // Inicjalizacja wyświetlacza LCD
  Serial.begin(9600);

  // Połączenie z Internetem za pomocą dostarczonej przez nas MAC i ustawionego adresu IP
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Błąd połączenia z DHCP");
    while (true);
  }

  delay(2000);

  // Połączenie i wysłanie zapytania do strony internetowej
  if (client.connect("www.example.com", 80)) {
    Serial.println("Połączono z serwerem");
    client.println("GET /index.php?page=example HTTP/1.0");

    client.println();
  } 
  else {
    Serial.println("Błąd połączenia z serwerem");
  }
}
```

Następnie możemy użyć funkcji HTMLParser.parse () aby wyodrębnić interesujące nas elementy kodu HTML. Przykładowy kod:

```Arduino
HTMLParser::HTMLTagCallback start_cb(void *cb_data, const char *tag_name, size_t tag_len)
{
  if (tag_name && tag_len == 2) {
    Serial.print("Znaleziono tag: ");
    Serial.print(tag_name, tag_len);
    Serial.print("\n");
  }

  if (tag_name == "a") {
    // Wyświetlamy linki na stronie
    ((bool *) cb_data)[0] = true;
  }

  return &start_cb;
}

void loop() {
  if (client.available()) {
    int len_out = 0;
    int count = 0;
    bool tag_found = false;

    do {
      int len = client.read(html_buffer, HTML_BUFSIZZ);

      len_out = HTMLParser::parse((char *) html_buffer, len,
        (void *) &tag_found, start_cb, /* default beaches */ NULL);

      Serial.println (html_buffer);
    } while (len_out > 0);
  }
}
```

Na powyższym przykładzie, wyodrębniamy wszystkie linki z kodu HTML i wyświetlamy je na ekranie. Dzięki temu prostemu kodowi, jesteśmy w stanie wyświetlić na ekranie informacje z dowolnego serwisu internetowego.

## Głębszy zanurzenie

Parsowanie HTML za pomocą Arduino może otworzyć wiele możliwości. Możemy wykorzystać to do monitorowania danych ze stron internetowych, wyświetlania informacji na wyświetlaczu LCD lub nawet tworzenia własnych interaktywnych stron internetowych z użyciem Arduino jako kontrolera.

Niezbędne jest jednak zdobyc