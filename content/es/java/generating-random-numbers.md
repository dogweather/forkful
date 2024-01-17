---
title:                "Generando números aleatorios"
html_title:           "Java: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

¿Qué y por qué?

Generar números aleatorios es un proceso en el que se crean secuencias de números al azar utilizando algoritmos matemáticos específicos. Los programadores usan esta técnica para agregar un elemento de azar en sus programas y juegos, ya sea para crear una experiencia más inmersiva o para realizar pruebas y cálculos estadísticos.

¿Cómo hacerlo?

En Java, podemos generar números aleatorios utilizando la clase 'Random'. Primero, debemos crear una instancia de la clase utilizando la palabra clave 'new'. Luego, podemos llamar al método 'nextInt()' para obtener un número entero aleatorio o al método 'nextDouble()' para obtener un número decimal aleatorio. A continuación, podemos especificar el rango de valores en el que queremos que se genere el número aleatorio mediante la inclusión de parámetros dentro de los paréntesis. Por ejemplo:

```Java
Random random = new Random(); // creamos una instancia de la clase Random
int numeroAleatorio = random.nextInt(100); // genera un número entero entre 0 y 99
double decimalAleatorio = random.nextDouble(10.5); // genera un número decimal entre 0.0 y 10.5
```

¡Y eso es todo! Ahora podemos utilizar estos números generados en nuestro código.

Profundizando

Los números aleatorios han sido utilizados a lo largo de la historia para propósitos científicos, de juego y de cifrado. En Java, también podemos utilizar la interfaz 'RandomAccess' para acceder a una posición aleatoria en una lista.

Otra forma de generar números aleatorios en Java es mediante el uso de la clase 'Math'. Esta clase incluye el método 'random()', el cual devuelve un número decimal aleatorio entre 0.0 y 1.0.

En cuanto a la implementación, es importante tener en cuenta que los números generados no son realmente aleatorios, ya que están determinados por un algoritmo predefinido. Sin embargo, pueden ser lo suficientemente impredecibles para ser utilizados en la mayoría de los casos.

Ver también

Si quieres profundizar en el tema de los números aleatorios en Java, puedes consultar la documentación oficial de la clase 'Random' y de la clase 'Math'.

También puedes explorar la implementación de números aleatorios en otros lenguajes de programación, como C++ o Python.