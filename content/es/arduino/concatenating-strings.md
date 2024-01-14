---
title:                "Arduino: Concatenando cadenas."
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# ¿Por qué usar concatenación de cadenas en Arduino?

La concatenación de cadenas es una técnica útil en Arduino que permite combinar dos o más cadenas de texto en una sola. Esto puede ser especialmente útil si estás trabajando con datos de sensores y quieres mostrarlos en una pantalla LCD o enviarlos por Bluetooth.

## Cómo usar la concatenación de cadenas en Arduino

Para concatenar cadenas en Arduino, puedes utilizar la función `+` o la función `concat()`. Por ejemplo:

```Arduino
// Ejemplo utilizando la función concat()
String nombre = "Juan";
String apellido = "García";
String nombre_completo = nombre.concat(apellido);
Serial.println(nombre_completo); // Salida: JuanGarcía

// Ejemplo utilizando la función +
String ciudad = "Madrid";
String pais = "España";
String ubicacion = ciudad + ", " + pais;
Serial.println(ubicacion); // Salida: Madrid, España
```

Como puedes ver, ambas funciones logran el mismo resultado. Sin embargo, es importante tener en cuenta que la función `concat()` modifica la cadena original, mientras que la función `+` crea una nueva cadena.

Además, también puedes utilizar la concatenación de cadenas para crear mensajes más complejos. Por ejemplo:

```Arduino
int temperatura = 25;
String mensaje = "La temperatura actual es: " + String(temperatura) + "°C";
Serial.println(mensaje); //Salida: La temperatura actual es: 25°C
```

En este caso, se utilizó la función `String()` para convertir la variable `temperatura` en una cadena antes de concatenarla con las demás cadenas.

## Profundizando en la concatenación de cadenas

En Arduino, las cadenas de texto son en realidad objetos del tipo `String`. Esto significa que, además de las funciones `concat()` y `+`, también puedes utilizar otros métodos que vienen incluidos en este tipo de objeto.

Algunos de estos métodos son:

- `substring()` para extraer una parte de una cadena.
- `charAt()` para obtener un carácter específico de una cadena.
- `toInt()` para convertir una cadena en un número entero.

La concatenación de cadenas también es útil cuando trabajas con arrays de caracteres. Por ejemplo:

```Arduino
char nombre[] = "María";
char apellido[] = "Gómez";
char nombre_completo[11];
strcat(nombre_completo, nombre); // Se agrega "María" a nombre_completo
strcat(nombre_completo, apellido); // Se agrega "Gómez" a nombre_completo
Serial.println(nombre_completo); // Salida: MaríaGómez
```

Aquí, se utilizó la función `strcat()` para concatenar los arrays de caracteres y crear una nueva cadena.

## Ver También
- Documentación oficial de Arduino sobre la concatenación de cadenas: https://www.arduino.cc/reference/en/language/variables/data-types/string/manipulation/
- Tutorial sobre concatenación de cadenas en Arduino: https://www.tutorialspoint.com/arduino/arduino_string_concat.htm
- Video tutorial sobre concatenación de cadenas en Arduino: https://www.youtube.com/watch?v=at8QCXhJW7E