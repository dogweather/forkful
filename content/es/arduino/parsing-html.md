---
title:                "Analizando html"
html_title:           "Arduino: Analizando html"
simple_title:         "Analizando html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué? 

Parsing HTML es el proceso de analizar y estructurar el código HTML de una página web para poder manipularlo y extraer información específica. Los programadores usan este proceso para automatizar tareas como extraer datos de una página web o crear contenido dinámico.

## Cómo hacerlo: 

Para analizar y parsear HTML en Arduino, podemos utilizar la librería HTMLino. A continuación se muestra un ejemplo de código que extrae el título de una página web y lo imprime en el monitor serial:

```
#include <HTMLino.h>

HTMLNode* root;

void setup() {
  Serial.begin(9600);
  HTMLino.get("https://www.ejemplo.com", root); // URL de la página a analizar
}

void loop() {
  if(HTMLino.update(root)){
    Serial.println(root->readInner("title")); // extrae el título de la página y lo imprime en el monitor serial
  }
}
```

La salida en el monitor serial sería algo como "Ejemplo de página web".

## Profundizando:

La necesidad de analizar y parsear HTML surgió con el auge de la web en la década de 1990. En aquel entonces, se requería procesar manualmente el código HTML para extraer información, lo que era un proceso tedioso y propenso a errores. La alternativa a utilizar una librería como HTMLino sería escribir nuestro propio código para analizar y estructurar el código HTML, lo que es mucho más complejo y requiere un conocimiento avanzado de programación.

La librería HTMLino es compatible con HTML 3.2, 4.0 y 5.0, lo que nos permite analizar páginas web modernas sin problemas. Además, también permite manipular el código HTML, por ejemplo, para agregar o eliminar elementos.

## Ver también:

- [Documentación de la librería HTMLino](https://arduinojson.org/)
- [Tutorial de HTMLino](https://randomnerdtutorials.com/parsing-html-arduino-esp8266-esp-32/)
- [Ejemplos de código con HTMLino](https://github.com/ekolodenko/HTMLino/tree/master/examples)