---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Una solicitud HTTP es un protocolo que permite a tu Arduino comunicarse con la web. Los programadores lo usan para interactuar con APIs, descargar información, y enviar datos a servidores.

## Cómo hacerlo:

Asegúrate de que tu Arduino esté conectado a la red con el módulo WiFi. Aquí, usaremos el WiFiClient de la biblioteca ESP8266WiFi.

```Arduino
#include <ESP8266WiFi.h>
 
const char* ssid     = "tuSSID";
const char* password = "tuPASSWORD";
 
const char* host = "ejemplo.com";
 
void setup() {
  
  Serial.begin(115200);
  
  delay(10);
 
  // Conexión a la red WiFi.
  Serial.println();
  Serial.println();
  Serial.print("Conectando a ");
  Serial.println(ssid);

  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
 
  Serial.println("");
  Serial.println("WiFi conectado");  
  Serial.println("Dirección IP: ");
  Serial.println(WiFi.localIP());
}
 
void loop() {
 
  WiFiClient client;
  
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Conexión fallida");
    return;
  }
  
  client.println("GET / HTTP/1.1");
  client.println("Host: " + String(host));
  client.println("Connection: close");
  client.println();
  
  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}
```

## Buceo profundo:

El Protocolo de Transferencia de Hipertexto (HTTP) se introdujo en 1991 como un estándar para la comunicación en el internet y se ha ido actualizando desde entonces. Existen bibliotecas alternativas para Arduino como EthernetClient y GSMClient para diferentes formas de conectividad. Además, al enviar una solicitud HTTP, considera los detalles de implementación, como los encabezados HTTP y la formación de la URL.

## Ver también:

Para más información y ejemplos detallados sobre peticiones HTTP con Arduino, consulta las siguientes fuentes:

- Documentación oficial de Arduino: http://arduino.cc/en/Reference/EthernetClient

- Tutorials para peticiones web con Arduino: https://randomnerdtutorials.com/esp8266-web-client/

- Esp32 HTTP Requests: https://techtutorialsx.com/2017/12/09/esp32-arduino-http-server-getting-query-parameters/