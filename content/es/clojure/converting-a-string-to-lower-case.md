---
title:    "Clojure: Convirtiendo una cadena a minúsculas"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una tarea común en la programación, ya que puede ser útil en muchas situaciones. Por ejemplo, puede ser necesario para la comparación de cadenas de texto, para normalizar los datos o simplemente para mejorar la legibilidad del código.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Clojure, puedes usar la función `clojure.string/lower-case` de la biblioteca estándar. A continuación, se muestra un ejemplo de cómo convertir una cadena a minúsculas y mostrar el resultado en la consola:

```Clojure
(clojure.string/lower-case "Hola MUNDO") ; output: "hola mundo"
```

También puedes usar la función `lower-case` de la biblioteca `clojure.string` en lugar de importar toda la biblioteca:

```Clojure
(require '[clojure.string :as str])
(str/lower-case "Hello WORLD") ; output: "hello world"
```

Ten en cuenta que la función `lower-case` no altera la cadena original, sino que devuelve una nueva cadena en minúsculas. Por lo tanto, es importante asignar el resultado a una nueva variable si deseas utilizar la cadena en minúsculas en tu código.

## Profundizando

Ahora que sabes cómo convertir una cadena a minúsculas en Clojure, es importante tener en cuenta algunas cosas más al usar esta función.

En primer lugar, la función `lower-case` solo funciona con caracteres ASCII. Esto significa que si tu cadena contiene caracteres Unicode, no se convertirán a minúsculas. Si necesitas convertir una cadena con caracteres Unicode a minúsculas, puedes utilizar la función `str/lower-case` de la biblioteca `clojure.string` mencionada anteriormente.

Además, la función `lower-case` utiliza la configuración regional predeterminada del sistema para determinar cómo convertir los caracteres en minúsculas. Por lo tanto, si estás trabajando con caracteres en un idioma diferente al configurado en tu sistema, es posible que no obtengas los resultados esperados. En este caso, puedes pasar una configuración regional específica como un tercer argumento a la función, por ejemplo `str/lower-case "HELLO" nil (java.util.Locale. :fr_FR)` para convertir "HELLO" a "hello" en el idioma francés.

## Ver también

- Documentación oficial de la función `clojure.string/lower-case`: https://clojuredocs.org/clojure.string/lower-case
- Tutorial de Clojure para principiantes: https://devnotesblog.wordpress.com/2013/03/23/clojure-tutorial/
- Ejemplos de uso de la función `lower-case`: https://www.programcreek.com/2013/07/clojure-function-for-changing-cases-of-a-string/