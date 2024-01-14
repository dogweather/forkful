---
title:                "Arduino: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

El concatenar cadenas de texto en un programa de Arduino puede ser muy útil para crear mensajes personalizados o para mostrar datos dinámicos en una pantalla. También puede ayudar a simplificar la escritura de código al permitir la reutilización de variables.

## Cómo hacerlo

Para concatenar cadenas de texto en Arduino, se puede utilizar la función "print". Esta función permite agregar variables, números o texto, todo en una sola línea. Por ejemplo:

```Arduino
int temperatura = 25;
float humedad = 50.5;
char* unidad = "%";

print("La temperatura actual es: ");
print(temperatura);
print("°C y la humedad es: ");
print(humedad);
print(unidad);
```

La salida de este código sería:

```
La temperatura actual es: 25°C y la humedad es: 50.50%
```

Si se desea guardar la cadena concatenada en una variable, se puede utilizar la función "String". Por ejemplo:

```Arduino
int temperatura = 25;
float humedad = 50.5;
char* unidad = "%";

String mensaje = "La temperatura actual es: ";
mensaje = mensaje + temperatura + "°C y la humedad es: " + humedad + unidad;
```

La variable "mensaje" ahora contendrá la cadena completa.

## Profundizando

En Arduino, cada vez que se concatena una cadena de texto, se está creando un nuevo objeto de tipo String. Estos objetos consumen memoria, por lo que se debe tener cuidado al usar la función "String" en programas con limitaciones de memoria.

Otra forma de concatenar cadenas es utilizando la función "strcat". Esta función toma dos cadenas existentes y las une juntas. Sin embargo, esta función es más limitada y solo puede concatenar una cadena al final de otra.

## Ver también

- [Documentación de Arduino sobre la función "print"](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Ejemplo de uso de la función "strcat"](https://electrosome.com/concatenate-two-strings-arduino/)
- [Tutorial sobre concatenación de cadenas en Arduino](https://www.electronicshub.org/string-concatenation-in-arduino/)