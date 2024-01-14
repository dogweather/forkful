---
title:    "Ruby: Buscando y remplazando texto"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

##Por qué

La búsqueda y reemplazo de texto es una herramienta útil para los programadores de Ruby. Permite encontrar y sustituir cadenas de texto en un archivo o documento de manera eficiente, ahorrando tiempo y esfuerzo.

##Cómo hacerlo

Para realizar una búsqueda y reemplazo de texto en Ruby, podemos utilizar el método `gsub` en un objeto de tipo `String`. Este método toma dos parámetros: la cadena de texto que deseamos reemplazar y la cadena de texto con la que queremos sustituirla. Por ejemplo:

```Ruby
texto = "Hola mundo!"
texto.gsub("mundo", "Ruby") # resultado: "Hola Ruby!"
```

También podemos utilizar expresiones regulares para realizar una búsqueda y reemplazo más avanzada. Por ejemplo, podemos utilizar la expresión regular `\d+` para reemplazar todos los números en una cadena de texto con un asterisco `*`.

```Ruby
texto = "Tengo 29 años."
texto.gsub(/\d+/, "*") # resultado: "Tengo * años."
```

##Profundizando

La búsqueda y reemplazo de texto en Ruby también nos permite utilizar bloques de código para realizar una sustitución más compleja. Por ejemplo, podemos convertir todas las palabras en una cadena de texto a mayúsculas mediante el uso del método `gsub!`. Este método toma un bloque como parámetro y nos permite modificar directamente el objeto original. Por ejemplo:

```Ruby
texto = "Hola mundo!"
texto.gsub!(/\w+/, &:upcase) # resultado: "HOLA MUNDO!"
```

Otro método útil es `sub`, que funciona de manera similar a `gsub` pero solo reemplaza la primera ocurrencia de la cadena de texto que proporcionamos. También podemos utilizar un bloque para realizar una sustitución más avanzada.

## Ver también

- [La documentación oficial de Ruby sobre el método `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Tutorial de búsqueda y reemplazo en Ruby](https://www.rubyguides.com/2019/07/ruby-gsub-method/)
- [Ejemplos de uso de expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)