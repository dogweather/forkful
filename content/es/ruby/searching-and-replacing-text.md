---
title:    "Ruby: Buscando y reemplazando texto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a buscar y reemplazar texto en Ruby?

La búsqueda y el reemplazo de texto son habilidades esenciales para cualquier programador de Ruby. Aprender a realizar estas funciones te ayudará a ahorrar tiempo y esfuerzo al manipular grandes cantidades de datos. Además, te permitirá ser más eficiente y productivo en tu trabajo.

## Cómo hacerlo:

Para buscar y reemplazar texto en Ruby, puedes utilizar el método `gsub()` de la clase `String`. Este método toma dos parámetros: la cadena de texto que deseas buscar y la cadena de texto por la que quieres reemplazarla. Aquí hay un ejemplo de cómo utilizarlo:

```Ruby
mi_cadena = "Hola, mundo!"
mi_cadena.gsub!("mundo", "amigos")
puts mi_cadena
```

El output de este código sería: `Hola, amigos!`. Como puedes ver, la cadena original se ha modificado con el nuevo texto. También puedes utilizar el método `sub()` para buscar y reemplazar solo la primera coincidencia.

## Profundizando:

La búsqueda y el reemplazo de texto en Ruby pueden ser aún más poderosos si utilizas expresiones regulares. Estas expresiones te permiten buscar patrones en lugar de una cadena de texto literal. Por ejemplo, si quisieras buscar y reemplazar todas las letras "a" seguidas de una "b" en una cadena, podrías utilizar la siguiente expresión regular: `a(b)+`, donde el signo "+" indica que la "b" puede aparecer una o más veces.

Además, también puedes utilizar el modificador `i` para hacer la búsqueda insensible a mayúsculas o minúsculas. Esto significa que si buscas la letra "a" con este modificador, también se coincidirán "A" o "a".

## Véase también:

Para seguir aprendiendo sobre las expresiones regulares en Ruby, te recomendamos revisar la documentación oficial de Ruby y probar diferentes patrones de búsqueda y reemplazo en tu código. También puedes explorar la librería `regexp` para obtener más funciones avanzadas. ¡Buena suerte en tu aprendizaje de buscar y reemplazar texto en Ruby!