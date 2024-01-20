---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP es el acto de solicitar datos a un servidor utilizando el protocolo HTTP. Los programadores lo hacen para interactuar con APIs y otras formas de servicios basados en la web.

## Cómo se hace:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();

  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();
  return 0;
}
```
Salida del programa: Si el código es exitoso, no habrá ninguna salida. Si falla, imprimirá un error.

## Profundización

(1) En el contexto histórico, el envío de solicitudes HTTP se introdujo como una forma de comunicación entre cliente-servidor en la web desde la invención de HTTP en 1991. 

(2) Aquí se utiliza la biblioteca libcurl, pero hay alternativas como neon, serf y la biblioteca de sockets en bruto de los sistemas operativos. El uso de libcurl en C es común debido a su eficacia y soporte de varios protocolos.

(3) Cuando se envía una solicitud HTTP, se establece una conexión TCP en el nivel más bajo con el servidor. A continuación, la solicitud se ensambla en el formato requerido por el protocolo HTTP para solicitar los datos deseados.

##Referencias 

1. Biblioteca libcurl: [https://curl.haxx.se/libcurl/c/](https://curl.haxx.se/libcurl/c/)
2. Solicitudes HTTP: [https://developer.mozilla.org/es/docs/Web/HTTP/Overview](https://developer.mozilla.org/es/docs/Web/HTTP/Overview)
3. Apuntes: AWS para programadores en C: [https://aws.github.io/aws-sdk-cpp/guide/](https://aws.github.io/aws-sdk-cpp/guide/)