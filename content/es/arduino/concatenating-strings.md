---
title:    "Arduino: Uniendo cadenas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Arduino?

En programación, a menudo necesitamos combinar o unir múltiples cadenas de texto para crear un mensaje más complejo o para formatear una salida de datos. En el mundo de Arduino, también podemos encontrarnos en situaciones en las que necesitamos concatenar cadenas para mostrar información en una pantalla LCD o enviar datos a través de una conexión serial.

## Cómo hacerlo

La función `strcat()` en Arduino nos permite concatenar cadenas de texto de una manera sencilla. Primero, declaramos nuestras cadenas de texto y luego usamos la función `strcat()` para unirlas en una nueva cadena.

```Arduino
//declaración de cadenas de texto
char str1[] = "¡Hola ";
char str2[] = "amigos!";
char resultado[15]; //una cadena más grande para almacenar el resultado

strcat(resultado, str1); //unimos la primera cadena
strcat(resultado, str2); //unimos la segunda cadena

Serial.println(resultado); //imprimimos el resultado en el puerto serie
```

**Salida:** ¡Hola amigos!

Podemos utilizar la función `strcat()` para concatenar tantas cadenas como queramos, siempre y cuando usemos una cadena lo suficientemente grande para almacenar todas ellas.

## Profundizando en la concatenación de cadenas

Al concatenar cadenas en Arduino, es importante tener en cuenta algunos detalles. Por ejemplo, asegúrate de tener suficiente espacio en la cadena de destino para almacenar todas las cadenas que estás uniendo. Si la cadena de destino no es lo suficientemente grande, se pueden producir errores o resultados inesperados.

También es importante recordar que no podemos concatenar cadenas y variables numéricas directamente. La función `strcat()` solo funciona con cadenas de texto, por lo que deberás convertir cualquier variable numérica en una cadena antes de unirla a otras cadenas.

Otra cosa a tener en cuenta es que, al unir una variable numérica con una cadena de texto, la variable será convertida en una cadena y se unirá al final de la cadena de texto. Por ejemplo:

```Arduino
//declaración de cadenas de texto
char str1[] = "Tengo ";
int x = 3;
char str2[] = " manzanas.";
char resultado[30]; //una cadena más grande para almacenar el resultado

strcat(resultado, str1); //unimos la primera cadena
strcat(resultado, x); //concatenamos la variable numérica (será convertida en una cadena)
strcat(strcat, str2); //unimos la segunda cadena

Serial.println(resultado); //imprimimos el resultado en el puerto serie
```

**Salida:** Tengo 3 manzanas.

Para obtener más información sobre cómo trabajar con cadenas de texto en Arduino, consulta la sección de documentación de la función `strcat()` [aquí](https://www.arduino.cc/reference/en/language/functions/string-and-constant-class/strcat/).

## Ver también

- [Cómo trabajar con cadenas de texto en Arduino](https://www.arduino.cc/en/Tutorial/String)
- [La función `strcat()` en la documentación de Arduino](https://www.arduino.cc/reference/en/language/functions/string-and-constant-class/strcat/)