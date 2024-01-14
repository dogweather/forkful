---
title:    "Ruby: Uso de expresiones regulares"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## ¿Por qué usar expresiones regulares en Ruby?

Las expresiones regulares son una poderosa herramienta en Ruby que permite buscar y manipular patrones de texto de manera eficiente. Al utilizarlas correctamente, se pueden ahorrar muchas líneas de código y tiempo de desarrollo. Además, son una habilidad útil en la mayoría de los lenguajes de programación, por lo que dominarlas en Ruby puede ser beneficioso para cualquier programador.

## Cómo utilizar expresiones regulares en Ruby

Las expresiones regulares en Ruby se escriben entre diagonales (`/`), y deben ir seguidas de un carácter que indica el tipo de búsqueda a realizar. Por ejemplo, `/hola/` buscará la palabra "hola" en el texto y devolverá una coincidencia si la encuentra.

Para utilizar expresiones regulares en Ruby, se utiliza el método `match` o `=~` junto con una cadena de texto y la expresión regular correspondiente. Por ejemplo:

```
texto = "¡Hola, cómo estás?"
if texto.match(/hola/)
  puts "Se encontró la palabra hola en el texto."
else
  puts "La palabra hola no se encontró en el texto."
end
# Output: Se encontró la palabra hola en el texto.
```

Además de `match`, existen otros métodos útiles como `scan`, que permite iterar sobre todas las coincidencias encontradas en una cadena, y `sub` o `gsub` que sirven para reemplazar texto basado en una expresión regular. 

## Profundizando en el uso de expresiones regulares

Las expresiones regulares en Ruby también permiten utilizar ciertos caracteres especiales para buscar diferentes patrones en el texto. Por ejemplo, el punto (`.`) se utiliza para encontrar cualquier carácter, excepto una nueva línea, mientras que el asterisco (`*`) se utiliza para indicar que se busca una coincidencia de cero o más veces. Por ejemplo:

```
texto = "Hola, hola, hola, hola"
puts texto.gsub(/ho.*?la/, "adiós")
# Output: adiós, adiós, adiós, adiós
```

Otro aspecto importante a tener en cuenta es que las expresiones regulares en Ruby son sensibles a mayúsculas y minúsculas, por lo que `/hola/` y `/HOLA/` serán consideradas diferentes expresiones regulares.

Además, existen algunos métodos útiles para manipular expresiones regulares, como `Regexp.union` que permite juntar dos expresiones regulares en una sola y `Regexp.escape` que escapa todos los caracteres especiales de una cadena para que puedan ser utilizados literalmente en una expresión regular.

## Ver también

- [Documentación oficial de expresiones regulares en Ruby](https://ruby-doc.org/core/Regexp.html)
- [Expresiones regulares en Rails](https://guides.rubyonrails.org/v5.2/active_support_core_extensions.html#regexp)
- [Expresiones regulares en otros lenguajes: Python](https://realpython.com/regex-python/)
- [Tutorial interactivo de expresiones regulares en Rubular](https://rubular.com/)