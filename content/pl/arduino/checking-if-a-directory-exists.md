---
title:                "Sprawdzanie istnienia katalogu."
html_title:           "Arduino: Sprawdzanie istnienia katalogu."
simple_title:         "Sprawdzanie istnienia katalogu."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Czego to dotyczy & dlaczego?
Sprawdzanie, czy istnieje katalog w programowaniu oznacza weryfikację czy podana ścieżka jest ważna i czy można do niej uzyskać dostęp. Programiści wykonują tę czynność, aby upewnić się, że dane, na których operują są prawidłowe i nie ma ryzyka błędnej lub niekompletnej obsługi.

Jak to zrobić:
W Arduino istnieją kilka sposobów, aby sprawdzić istnienie katalogu. Można to zrobić poprzez zdalne połączenie z komputerem za pomocą protokołu SSH lub wykorzystując bibliotekę ESP8266HTTPClient. Poniżej przedstawione są dwa przykłady kodu dla każdego sposobu.

Arduino + SSH:
```
#include <WiFi.h>
#include <WiFiClient.h>
#include <SSSH.h>

const char* ssid = "nazwa_sieci";
const char* password = "haslo_sieci";

void setup() {
  Serial.begin(115200);
  WiFi.mode(WIFI_STA);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to wifi");
  Serial.println("Opening ssh connection");
  client.connect("ip_serwera", 22, "uzytkownik", "haslo");
  if (client.connected())
    Serial.println("SSH connection opened");
  else
    Serial.println("Connection failed");
}

void loop() {
  ArduinoSSH ssh;
  if (ssh.exec(&client, "ls katalog") > 0)
    Serial.println("Katalog istnieje");
  else
    Serial.println("Katalog nie istnieje");
}
```

Arduino + ESP8266HTTPClient:
```
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "nazwa_sieci";
const char* password = "haslo_sieci";

String url = "http://ip_serwera/katalog";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to wifi");
}

void loop() {
  HTTPClient http;
  http.begin(url);
  int code = http.GET();
  if (code)
    Serial.println("Katalog istnieje");
  else
    Serial.println("Katalog nie istnieje");
  http.end();
}
```

Głębokie zanurzenie:
Sprawdzanie istnienia katalogu jest niezbędnym elementem przy manipulacji plikami w systemie operacyjnym. Wcześniej programiści używali funkcji poprawnych z systemem operacyjnym, takich jak "stat" w Linuxie czy "getfileattibutes" w Windowsie. Jednak w przypadku systemów wbudowanych, takich jak Arduino, wymagało to użycia zewnętrznych bibliotek lub modułów, jak na przykład ESP8266HTTPClient. Obecnie, dzięki rozwojowi technologii, możliwe jest wykonywanie tej operacji bezpośrednio w Arduino, co ułatwia programowanie i zwiększa elastyczność.

Zobacz też:
- https://www.arduino.cc/en/Main/ArduinoSSH
- https://github.com/esp8266/Arduino/blob/master/libraries/ESP8266HTTPClient/examples/HTTPClient/BasicHTTPClient/BasicHTTPClient.ino