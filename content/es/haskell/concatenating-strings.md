---
title:    "Haskell: Uniendo cadenas de texto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica común en la programación que permite unir dos o más cadenas de texto en una sola. Esto puede ser útil en diversas situaciones, como cuando se desea mostrar una oración completa en lugar de varias partes separadas. También puede ser útil para crear nombres de archivos dinámicamente o generar mensajes personalizados para los usuarios.

## Cómo hacerlo

La concatenación de cadenas en Haskell es sencilla y se puede lograr utilizando el operador `++` o la función `concat`. A continuación se muestra un ejemplo de ambas formas:

```Haskell
-- usando el operador ++
"¡Hola " ++ "mundo!" -- resulta en "¡Hola mundo!"

-- usando la función concat con una lista de cadenas
concat ["¡Hola", "mundo", "!"] -- resulta en "¡Hola mundo!"
```

Tenga en cuenta que el operador `++` solo puede usarse para concatenar dos cadenas a la vez, mientras que la función `concat` puede unir una lista de cadenas.

## Profundizando

En Haskell, las cadenas son en realidad listas de caracteres, lo que significa que se pueden utilizar las mismas funciones que se usan para trabajar con listas en general. Por ejemplo, se puede usar la función `map` para modificar cada carácter de una cadena antes de concatenarla con otra. Aquí hay un ejemplo:

```Haskell
-- función que convierte una letra mayúscula en minúscula
toLower :: Char -> Char
toLower c = toEnum( fromEnum c + 32)

-- concatenación de cadenas con función map
map toLower ("HOLA " ++ "AMIGOS!") -- resulta en "hola amigos!"
```

También es importante tener en cuenta que la concatenación de cadenas en Haskell es una operación costosa en términos de tiempo de ejecución. Esto se debe a que una nueva cadena se crea cada vez que se realiza la concatenación. Por lo tanto, si se necesitan realizar múltiples concatenaciones, puede ser más eficiente usar la función `concat` con una lista de cadenas.

## Ver también

- [Documentación oficial de Haskell sobre concatenación de cadenas](https://www.haskell.org/documentation/#strings)
- [Tutorial sobre concatenación de cadenas en Haskell](https://wiki.haskell.org/Strings)
- [Ejemplos de concatenación de cadenas en Haskell](https://www.programminghaskell.com/concatenating-strings-with-haskell/)