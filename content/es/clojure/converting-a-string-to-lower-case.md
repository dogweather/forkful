---
title:    "Clojure: Convirtiendo una cadena a minúsculas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser útil en muchas situaciones. Puede que necesites comparar dos cadenas de texto, pero no quieras que las letras mayúsculas o minúsculas afecten al resultado. O tal vez estés trabajando con datos sensibles y quieras asegurarte de que todo se encuentra en el mismo formato para facilitar su manipulación. Independientemente del motivo, es importante saber cómo realizar esta tarea en Clojure.

## Cómo hacerlo

La forma más sencilla de convertir una cadena de texto a minúsculas en Clojure es utilizando la función `lower-case`. Esta función toma una cadena de texto como argumento y devuelve una nueva cadena en minúsculas. Veamos un ejemplo:

```Clojure
(lower-case "Hola MUNDO") ; devuelve "hola mundo"
```

Si queremos trabajar con una colección de cadenas de texto, podemos utilizar la función `map` para aplicar la función `lower-case` a cada uno de los elementos. Por ejemplo:

```Clojure
(map lower-case ["Hola", "MUNDO"]) ; devuelve ("hola" "mundo")
```

## Profundizando

Es importante tener en cuenta que la función `lower-case` utiliza la configuración del sistema para determinar qué caracteres deben ser convertidos a minúsculas. Por lo tanto, es posible que la salida de esta función varíe dependiendo del sistema en el que se ejecute el código.

Además, también existe la función `clojure.string/lower-case`, que proporciona una funcionalidad similar a `lower-case`. Sin embargo, esta función utiliza las normas de UNICODE para determinar qué caracteres deben ser convertidos a minúsculas, en lugar de depender de la configuración del sistema.

## Ver también

- [La documentación oficial de la función lower-case](https://clojuredocs.org/clojure.core/lower-case)
- [La documentación oficial de la función clojure.string/lower-case](https://clojuredocs.org/clojure.string/lower-case)