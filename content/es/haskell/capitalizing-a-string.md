---
title:    "Haskell: Capitalizando una cadena"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena es importante en la programación para asegurarse de que la presentación de los datos sea coherente y fácil de leer. Ya sea que estés trabajando en un proyecto personal o en un equipo de desarrollo, capitalizar una cadena puede mejorar la legibilidad del código y facilitar su mantenimiento en el futuro.

## Cómo hacerlo

Para capitalizar una cadena en Haskell, se puede usar la función `toUpper` de la librería `Data.Char`. Esta función convierte un carácter en su equivalente en mayúsculas. En combinación con la función `map`, la cual aplica una función a cada elemento de una lista, podemos capitalizar una cadena completa.

```Haskell
import Data.Char (toUpper)

capitalize :: [Char] -> [Char]
capitalize str = map toUpper str

main = do
  putStrLn "Ingresa una cadena:"
  str <- getLine
  putStrLn ("La cadena capitalizada es: " ++ capitalize str)
```

Ejemplo de entrada: "hola mundo"
Salida esperada: "HOLA MUNDO"

## Profundizando

La función `toUpper` solo funciona con caracteres del alfabeto latino, por lo que si tu cadena contiene caracteres especiales o de otros idiomas, puede ser necesario implementar una función más robusta para capitalizarla correctamente.

También es importante tener en cuenta que la función `map` devuelve una nueva lista con los elementos modificados, en lugar de modificar la lista original. Por lo tanto, si se desea modificar directamente la cadena, se puede usar la función `fmap` junto con `toUpper`.

Por último, cabe mencionar que hay múltiples formas de capitalizar una cadena en Haskell utilizando diferentes combinaciones de funciones y librerías. Explorar y experimentar con distintas opciones puede ser una forma divertida y útil de aprender más sobre el lenguaje.

## Ver También

- Documentación de `Data.Char` en Hackage: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html
- Tutorial sobre funciones de orden superior en Haskell: https://www.youtube.com/watch?v=Jg3VgJ-K-i0
- Artículo sobre capitalizar una cadena en Haskell: https://www.joachim-breitner.de/blog/594-Some_toughts_on_capitalizing_strings_in_Haskell