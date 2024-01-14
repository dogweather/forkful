---
title:                "Arduino: Encontrando la longitud de una cadena"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué necesitamos calcular la longitud de una cadena en Arduino?

En la programación de Arduino, a menudo necesitamos conocer la longitud de una cadena o texto para poder realizar ciertas operaciones, como mostrar mensajes en una pantalla LCD, transmitir datos a través de Bluetooth o almacenar información en una tarjeta SD. Calcular la longitud de una cadena es una habilidad esencial para cualquier proyecto en Arduino que involucre el manejo de datos.

## Cómo calcular la longitud de una cadena en Arduino

Para encontrar la longitud de una cadena en Arduino, podemos usar la función `strlen()`, que se encuentra en la biblioteca estándar de C. Esta función recibe una cadena como argumento y devuelve un valor entero que representa la cantidad de caracteres en la cadena.

```Arduino
char str[] = "Hola, mundo!"; // cadena de ejemplo
int len = strlen(str); // len tendrá un valor de 12
```

Podemos también usar la función `sizeof()`, que devuelve el número de bytes ocupados por una variable, incluyendo el carácter nulo al final de la cadena.

```Arduino
char str[] = "Hola, mundo!";
int len = sizeof(str); // len tendrá un valor de 13
```

Otra opción es utilizar un bucle `for` para recorrer la cadena y contar la cantidad de caracteres.

```Arduino
char str[] = "Hola, mundo!";
int len;

for (int i = 0; str[i] != '\0'; i++) {
  len++;
}

// len tendrá un valor de 12
```

## Profundizando en la función `strlen()`

La función `strlen()` se basa en la siguiente lógica: recorre una cadena caracter por caracter hasta encontrar el carácter nulo `\0`, que indica el final de la cadena. Cada vez que encuentra un carácter, incrementa un contador en uno. Al finalizar el recorrido, devuelve el valor del contador, que representa la longitud de la cadena.

En caso de que la cadena esté vacía, la función devuelve un valor de 0. Y si la cadena no tiene un carácter nulo al final, puede haber errores en su ejecución.

## Vea también

- [Documentación de la función `strlen()` en el sitio web oficial de Arduino](https://www.arduino.cc/reference/en/language/functions/string/functions/strlen/)
- [Ejemplo de uso de `strlen()` en un proyecto de Arduino](https://www.hackster.io/arunoda-meegahapola/find-the-length-of-a-string-in-arduino-d0cf74)