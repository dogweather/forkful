---
title:    "Clojure: Escribiendo en error estándar"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# ¿Por qué escribir a la salida de error estándar en Clojure?

Escribir a la salida de error estándar puede ser una tarea útil para los programadores de Clojure. Puede ser utilizado para imprimir mensajes de error específicos para ayudar en la depuración de código o simplemente para mostrar información adicional durante la ejecución de un programa.

# Cómo hacerlo

Para escribir a la salida de error estándar en Clojure, simplemente use la función `println` y pase el mensaje que desea imprimir como argumento. Por ejemplo, si desea imprimir el mensaje "Error: Valor inválido", puede escribirlo de la siguiente manera:

```Clojure
(println "Error: Valor inválido")
```

Esto producirá el siguiente resultado:

```
Error: Valor inválido
```

También puede combinar la función `println` con otras funciones o variables para imprimir mensajes más complejos.

# Deep Dive

Cuando se utiliza la función `println` para escribir a la salida de error estándar, es importante tener en cuenta que la salida se imprimirá en una nueva línea después del mensaje. Si desea imprimir un mensaje en la misma línea que otro contenido, puede utilizar la función `print`.

Además, también puede utilizar la función `eprint` para imprimir a la salida de error estándar sin agregar una nueva línea al final del mensaje. Esto puede ser especialmente útil cuando se necesita imprimir mensajes de error en medio de la ejecución de un programa.

# Ver También

- [La documentación oficial de Clojure](https://clojure.org/)
- [Tutorial de Clojure en Español](https://clojure.org/)
- [Artículo sobre la salida de error estándar en Clojure](https://www.baeldung.com/clojure-error-handling)