---
title:    "Arduino: Convirtiendo una fecha en una cadena"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Si estás trabajando en un proyecto de Arduino que necesita mostrar la fecha actual, es posible que te encuentres con la necesidad de convertir una fecha en una cadena. Al convertir una fecha en una cadena, puedes mostrarla fácilmente en una pantalla LCD o enviarla por una conexión serial a otro dispositivo.

## Cómo hacerlo: 

Aquí hay un ejemplo de cómo convertir una fecha en una cadena en Arduino:

```Arduino
#include <TimeLib.h>  // Librería de tiempo

void setup() {
  // Inicializar la conexión serial
  Serial.begin(9600);
  // Definir la fecha y hora actual
  tmElements_t tm;
  tm.Year = 2021;
  tm.Month = 9;
  tm.Day = 15;
  tm.Hour = 15;
  tm.Minute = 30;
  tm.Second = 0;
  // Convertir la fecha y hora en una variable de tipo time_t
  time_t t = makeTime(tm);
  // Crear una cadena que contenga la fecha
  char dateStr[11]; // Espacio para 11 caracteres: "dd/mm/yyyy\0"
  sprintf(dateStr, "%02d/%02d/%04d", day(t), month(t), year(t));
  // Imprimir la fecha en la consola serial
  Serial.println(dateStr);
}

void loop() {
  // No es necesario nada en el bucle loop para este ejemplo
}
```

#### Salida:

```
15/09/2021 // Muestra la fecha en formato dd/mm/yyyy
```

## Profundizando:

La función `sprintf()` utilizada en el ejemplo es una herramienta poderosa para convertir variables en cadenas formateadas. El primer parámetro es la cadena de formato que se utilizará para crear la nueva cadena. Luego, en los siguientes parámetros, proporcionamos las variables que se deben agregar a la cadena siguiendo el formato especificado.

En nuestro caso, utilizamos `%02d`, `%02d` y `%04d` para especificar el formato de la cadena de fecha. Estos significan, respectivamente, que la variable debe ser un número entero de 2, de 2 y de 4 dígitos, con un cero agregado al principio si el número tiene menos dígitos.

Con esto, podemos cambiar fácilmente el orden en que se muestran los componentes de la fecha simplemente cambiando el orden de las variables en `sprintf()`.

## Ver también:

- [TimeLib Library Reference](https://www.arduino.cc/en/Reference/Time) 
- [sprintf() function reference](https://www.cplusplus.com/reference/cstdio/sprintf/) 
- [Como convierto una variable días en 1, 2, 3, etc](https://forum.arduino.cc/t/como-convierto-una-variable-dias-en-1-2-3-etc-dd-mm-yyyy/646815)