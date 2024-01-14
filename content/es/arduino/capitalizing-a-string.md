---
title:                "Arduino: Capitalizando una cadena"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a veces es necesario capitalizar una cadena de texto, es decir, convertir todas las letras minúsculas a mayúsculas. Puede ser útil al trabajar con bases de datos o al mostrar información en una pantalla LCD.

## Cómo hacerlo

Para capitalizar una cadena de texto en Arduino, podemos utilizar la función `toUpperCase()`. Esta función toma una letra como parámetro y devuelve la misma letra en mayúscula. Podemos utilizarla en un bucle `for` para recorrer cada letra de la cadena y aplicarla a una nueva cadena modificada, que será nuestra cadena capitalizada.

```Arduino
// Ejemplo de capitalización de una cadena de texto
String cadena = "hola mundo";
String cadenaCapitalizada = "";
for (int i = 0; i < cadena.length(); i++) {
    char letra = cadena.charAt(i); // Obtener cada letra de la cadena
    letra = toupper(letra); // Convertir la letra a mayúscula
    cadenaCapitalizada += letra; // Agregar la letra a la nueva cadena
}
Serial.println(cadenaCapitalizada); // Imprimir la cadena capitalizada: HOLA MUNDO
```

## Profundizando

Si queremos ser más eficientes, podemos evitar el uso de un bucle `for` utilizando el método `toUpperCase()` directamente en la cadena original. También podemos mejorar nuestra función para que pueda manejar letras con tildes o caracteres especiales. Esto se puede lograr utilizando un mapa de caracteres, donde se especifica la equivalencia entre cada letra minúscula y su versión mayúscula correspondiente.

```Arduino
// Ejemplo mejorado de capitalización de una cadena de texto
String cadena = "Hóla, ¿cómo estáś?";
String cadenaCapitalizada = "";
for (int i = 0; i < cadena.length(); i++) {
    char letra = cadena.charAt(i);
    letra = toupper(letra);
    // Comprobar si la letra original tenía un equivalente mayúscula en el mapa
    if (mapaDeCaracteres[letra]) {
        letra = mapaDeCaracteres[letra]; // Asignar la equivalencia correspondiente
    }
    cadenaCapitalizada += letra;
}
Serial.println(cadenaCapitalizada); // Imprimir la cadena capitalizada: HÓLA, ¿CÓMO ESTÁŚ?
```

## Ver también

- [Ejemplos de caracteres UTF-8 en Arduino](https://www.arduino.cc/en/Tutorial/Utf8ascii)
- [Referencia de la función `toUpperCase()`](https://www.arduino.cc/reference/en/language/functions/characters/touppercase/)