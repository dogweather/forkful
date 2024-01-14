---
title:    "Arduino: Utilizando expresiones regulares"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por qué usar expresiones regulares en Arduino

Si eres un programador de Arduino, es posible que hayas escuchado hablar sobre expresiones regulares. Pero, ¿qué son y por qué deberías usarlas en tus proyectos de Arduino? Las expresiones regulares son patrones de caracteres que se utilizan para buscar y manipular texto. Pueden ser muy útiles en muchos aspectos de la programación, y en Arduino, pueden ser especialmente útiles para validar entradas de usuario y manejar datos de sensores.

## Cómo utilizar expresiones regulares en Arduino

Para utilizar expresiones regulares en tus proyectos de Arduino, primero debes incluir la librería "Regex", que se encuentra en la biblioteca estándar de Arduino. Luego, puedes utilizar la función regex() para crear un objeto regex y especificar el patrón que deseas buscar. Por ejemplo, si deseas validar una dirección de correo electrónico, podrías hacerlo de la siguiente manera:

```Arduino
#include <Regex.h>

void setup() {
    // Crear un objeto regex y especificar el patrón
    Regex correo("\\w+@[a-zA-Z]+\\.[a-zA-Z]{2,3}");

    // Validar una dirección de correo electrónico
    if (correo.match("ejemplo@dominio.com")) {
        Serial.println("Dirección de correo electrónico válida");
    } else {
        Serial.println("Dirección de correo electrónico inválida");
    }
}

void loop() {

}
```

En este ejemplo, utilizamos el patrón "\w+@[a-zA-Z]+\.[a-zA-Z]{2,3}" que coincide con una dirección de correo electrónico válida. Puedes cambiar este patrón para adaptarlo a tus necesidades. La función match() devuelve un valor booleano que indica si el patrón coincide con la cadena de entrada o no.

También puedes utilizar expresiones regulares para manipular datos, por ejemplo, si deseas extraer solo los números de una cadena de entrada. En este caso, puedes utilizar la función extract() en lugar de match(). Por ejemplo:

```Arduino
#include <Regex.h>

void setup() {
    // Crear un objeto regex y especificar el patrón
    Regex numeros("\\d+");

    // Extraer los números de la cadena
    int num = numeros.extract("Hoy es 20 de enero y son las 9 de la mañana");

    // Imprimir resultado
    Serial.println(num);
}

void loop() {

}
```

En este caso, la función extract() devuelve el primer número que encuentra en la cadena de entrada. Puedes utilizar esta técnica para manipular y procesar datos de sensores en tus proyectos de Arduino.

## Más información sobre el uso de expresiones regulares

Si quieres profundizar en el uso de expresiones regulares en Arduino, puedes consultar la documentación oficial de la librería y buscar tutoriales en línea. También puedes experimentar y practicar con diferentes patrones para ver cómo afectan los resultados.

¡Ahora que ya sabes cómo utilizar expresiones regulares en Arduino, es hora de aplicar esta técnica en tus proyectos y aprovechar al máximo sus ventajas!

# Ver También

- [Documentación oficial de la librería Regex de Arduino](https://www.arduino.cc/reference/en/libraries/regex/)
- [Tutorial de Regex en Arduino](https://howtomechatronics.com/tutorials/arduino/regular-expression-library-regex-usage-tutorial/)
- [Ejemplos de expresiones regulares en Arduino](https://forum.arduino.cc/index.php?topic=328085.0)