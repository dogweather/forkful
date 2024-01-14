---
title:                "Ruby: Extrayendo subcadenas"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en Ruby

Extraer subcadenas es una habilidad crucial en la programación de Ruby que te permite manipular cadenas de texto de manera más eficiente y precisa. Ya sea que quieras buscar una palabra específica dentro de un texto o dividir una cadena en partes más pequeñas, saber cómo extraer subcadenas te ahorrará tiempo y errores en tu código.

## Cómo hacerlo

Para extraer subcadenas en Ruby, puedes utilizar el método `[]` o `slice()`. Estos métodos trabajan de manera similar, permitiéndote especificar el inicio y fin de la subcadena que deseas extraer.

```
texto = "Esto es una cadena de texto"
texto[0..3] # salida: "Esto"
texto.slice(5,2) # salida: "es"
```

También puedes utilizar expresiones regulares para extraer patrones específicos en una cadena de texto. Por ejemplo, si quieres encontrar todas las palabras que contengan la letra "a", puedes utilizar el método `scan()` junto con una expresión regular.

```
texto = "La manzana es una fruta saludable"
texto.scan(/[aA]\w+/) # salida: ["La", "manzana", "saludable"]
```

## Profundizando

Es importante recordar que los índices en Ruby empiezan en 0, por lo que para extraer el primer carácter de una cadena de texto, debes utilizar el índice 0. Además, si no se especifica un índice final, el método `[]` tomará la subcadena desde el índice inicial hasta el final de la cadena.

```
texto = "Hola Mundo!"
texto[3] # salida: "a"
texto[3..] # salida: "a Mundo!"
```
Puedes encontrar más información sobre cómo extraer subcadenas y todos los métodos disponibles en la documentación oficial de Ruby.

## Ver También

- [Documentación oficial de Ruby sobre métodos de cadena](https://ruby-doc.org/core-2.7.1/String.html)
- [Tutorial de extracción de subcadenas en Ruby](https://www.rubyguides.com/2019/10/ruby-substring/)
- [Expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)