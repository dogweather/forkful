---
title:    "C++: Concatenando cadenas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Una de las tareas más comunes en la programación es la concatenación de cadenas de texto. Esta técnica nos permite combinar varias cadenas en una sola, lo que es especialmente útil en la creación de mensajes personalizados o en la obtención de información de diferentes fuentes.

## Cómo hacerlo
La concatenación de cadenas en C++ es bastante sencilla. Utilizamos el operador "+" para unir dos o más cadenas y el resultado se guarda en una nueva variable. Por ejemplo:

```C++
string nombre = "Juan";
string apellido = "Pérez";
string nombre_completo = nombre + " " + apellido;
```

En este ejemplo, hemos creado tres variables: "nombre", "apellido" y "nombre_completo". La tercera variable contiene la concatenación de las dos primeras, junto con un espacio en blanco para separarlas.

También podemos usar la función "append" para agregar una cadena al final de otra. Por ejemplo:

```C++
string saludo = "¡Hola ";
string nombre = "Juan!";
saludo.append(nombre);
```

En este caso, la variable "saludo" se actualizará a "¡Hola Juan!".

En cuanto a la salida, podemos imprimir la cadena resultante en la consola utilizando la función "cout" de la biblioteca estándar de C++, o también podemos almacenarla en un archivo de texto para su posterior uso.

## Profundizando
La concatenación de cadenas puede volverse más complicada cuando se trabaja con diferentes tipos de variables, como números y caracteres especiales. En esos casos, es importante asegurarse de convertir las variables a tipo de datos de cadena antes de realizar la concatenación, para evitar errores en la salida.

Además, hay que tener en cuenta que la concatenación de cadenas puede tener un gran impacto en la eficiencia y el rendimiento del código. Cuando se realizan múltiples concatenaciones en bucles o procesos, puede ser más eficiente utilizar un enfoque diferente, como el uso de un constructor de flujo de cadenas.

## Ver también
- [Documentación oficial de C++ sobre la concatenación de cadenas] (https://es.cppreference.com/w/cpp/string/basic_string/operator%2B)
- [Ejemplos de concatenación de cadenas en C++] (https://www.delftstack.com/es/howto/cpp/concatenate-strings-in-cpp/)