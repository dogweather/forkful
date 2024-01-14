---
title:    "Go: Extrayendo subcadenas"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer substrings es una habilidad importante para cualquier programador de Go. Esto permite manipular cadenas de texto de manera eficiente y precisa, lo que facilita la realización de tareas como búsqueda, reemplazo y validación de datos. Además, es una técnica que se utiliza en muchos lenguajes de programación y tener conocimiento sobre cómo hacerlo en Go puede ayudar a ampliar tus habilidades como programador.

## Cómo hacerlo

Para extraer un substring en Go, se utiliza la función `Substr()` que se encuentra en el paquete `strings`. Esta función toma tres argumentos: la cadena de texto original, el índice donde se inicia el substring y la longitud del mismo. A continuación, se presenta un ejemplo de cómo utilizarla:

```Go
cadena := "Hola, ¿cómo estás?"
substr := strings.Substr(cadena, 6, 5)
```

En este caso, el substring empezaría a extraerse en la sexta posición de la cadena original y tendría una longitud de cinco caracteres, por lo que el valor de `substr` sería "cómo". También es posible utilizar valores negativos para empezar a extraer desde el final de la cadena, por ejemplo `strings.Substr(cadena, -4, 3)` extraería los últimos tres caracteres de la cadena, en este caso "tás". Asimismo, la función `Substr()` se puede utilizar para extraer múltiples substrings en una sola cadena, simplemente proporcionando los índices iniciales y las longitudes correspondientes.

## En profundidad

Es importante tener en cuenta que los índices en Go se inician en cero, por lo que el primer caracter de una cadena tiene el índice 0. Además, la función `Substr()` es sensible a las mayúsculas y minúsculas, por lo que si se utiliza en un substring, éste debe coincidir con la escritura exacta del texto original. También es posible utilizar la función `Substr()` en cadenas Unicode, lo que lo hace versátil para trabajar con diferentes idiomas.

## Ver también

- Documentación oficial de Go sobre la función `Substr()`: https://golang.org/pkg/strings/#Substring
- Ejemplos de uso de la función `Substr()`: https://gobyexample.com/slice

¡Ahora estás preparado para empezar a utilizar la función `Substr()` en tus proyectos de Go y aprovechar al máximo esta herramienta esencial en el manejo de cadenas de texto!