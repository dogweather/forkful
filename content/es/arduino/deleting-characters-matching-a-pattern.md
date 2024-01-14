---
title:                "Arduino: Eliminando caracteres que coinciden con un patrón"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón puede ser útil en situaciones donde queremos limpiar o manipular datos almacenados en variables, como por ejemplo, eliminar espacios en blanco o símbolos no deseados. También puede ayudar a mejorar el rendimiento de nuestro código al reducir la cantidad de datos con los que tiene que trabajar.

## Cómo

Para eliminar caracteres que coinciden con un patrón en Arduino, podemos utilizar la función `remove_if()` de la librería `String`. Esta función toma dos argumentos: una función o expresión lambda que define el patrón a buscar, y la variable en la que se realizarán los cambios. Aquí hay un ejemplo de cómo podemos usarlo para eliminar todos los espacios en blanco de una cadena:

```Arduino
#include <String.h>

void setup() {
  Serial.begin(9600);
  String text = "Esto es un ejemplo de texto con espacios en blanco.";
  Serial.println(text);
  text.remove_if([](char c){return c == ' ';}); //lambda que busca espacios en blanco
  Serial.println(text);
}

void loop() {

}
```

El resultado de este código sería:

```
Esto es un ejemplo de texto con espacios en blanco.
Esto es unejemplodetextoconespaciosenblanco.
```

También podemos utilizar la función `replace()` de la misma librería para reemplazar caracteres específicos en una cadena. Aquí hay un ejemplo de cómo podríamos usarlo para reemplazar todos los números con un asterisco:

```Arduino
#include <String.h>

void setup() {
  Serial.begin(9600);
  String text = "1, 2, 3, 4 y 5.";
  Serial.println(text);
  text.replace("1", "*");
  text.replace("2", "*");
  text.replace("3", "*");
  text.replace("4", "*");
  text.replace("5", "*");
  Serial.println(text);
}

void loop() {

}
```

El resultado sería:

```
1, 2, 3, 4 y 5.
*, *, *, * y *.
```

Es importante tener en cuenta que estas funciones solo se aplican a cadenas de caracteres, no a variables de otros tipos. Y si queremos aplicar cambios a una cadena existente, debemos almacenar el resultado en una nueva variable.

## Profundizando

Si queremos tener un control más preciso sobre qué caracteres queremos eliminar o reemplazar, podemos utilizar expresiones regulares con la librería `Regex`. Estas expresiones nos permiten buscar patrones específicos en una cadena, como por ejemplo, todos los números o letras mayúsculas. Aquí hay un ejemplo sencillo de cómo podríamos utilizar una expresión regular para buscar y eliminar todos los caracteres no numéricos en una cadena:

```Arduino
#include <String.h>
#include <regex.h>

void setup() {
  Serial.begin(9600);
  String text = "Texto con números 1, 2 y 3.";
  Serial.println(text);
  regex_t regex; //creamos la expresión regular
  int regcomp_result = regcomp(&regex, "[^0-9]+", REG_EXTENDED);
  text.remove_if([&regex](char c){return regexec(&regex, &c, 0, NULL, 0) == 0;}); //lambda que busca caracteres no numéricos
  Serial.println(text);
}

void loop() {

}
```

El resultado sería:

```
Texto con números 1, 2 y 3.
123.
```

Para aprender más sobre expresiones regulares y cómo utilizarlas en Arduino, te recomendamos revisar la documentación oficial y buscar ejemplos en línea.

## Ver también

- [Documentación de la función `remove_if()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove_if/)
- [Documentación de la función `replace()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Documentación de la librería `Regex`](https://github.com/nickgammon/Regex)