---
title:    "Clojure: Escribiendo a la salida de error estándar"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida estándar, también conocido como stderr, es una práctica común en la programación de Clojure. Esto permite a los desarrolladores imprimir mensajes de error o depuración en la consola durante la ejecución del programa. Aunque puede sonar simple, saber cómo escribir a stderr es una habilidad importante para los programadores de Clojure ya que les permite depurar y mejorar su código de manera efectiva.

## Cómo hacerlo

Para escribir a stderr en Clojure, se utiliza la función `println!` seguida de la expresión que se desea imprimir. Por ejemplo:

```Clojure
(println! "¡Hola mundo!")
```

Esto mostrará el mensaje "¡Hola mundo!" en la salida estándar.

Si quieres escribir algo más específico, puedes utilizar la función `println!` junto con la función `str` para crear una cadena de caracteres que se imprimirá. Por ejemplo:

```Clojure
(println! (str "La suma de 2 + 2 es " (+ 2 2)))
```

Esto imprimirá "La suma de 2 + 2 es 4" en la salida estándar.

## Profundizando

Es importante tener en cuenta que las impresiones a stderr se realizan de manera sincrónica, lo que significa que el programa se detendrá hasta que el mensaje se imprima antes de continuar con la ejecución. También se pueden utilizar las funciones `prn!` y `pr-str` para imprimir valores específicos y obtener una representación en cadena de caracteres respectivamente.

Otro aspecto importante a tener en cuenta es que stderr no solo se limita a mensajes de error o depuración, sino que también se puede utilizar para mostrar información importante durante el funcionamiento del programa, como el progreso de una operación.

## Ver también

- Documentación oficial sobre la función `println!`: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/println!
- Más información sobre la función `str`: http://clojuredocs.org/clojure.core/str
- Ejemplos prácticos de cómo escribir a stderr en Clojure: https://www.braveclojure.com/logging-and-exception-handling/

¡Ahora estás listo para utilizar la salida estándar en tus programas de Clojure de manera efectiva! Recuerda que es una herramienta útil para imprimir información y depurar tu código.