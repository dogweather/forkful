---
title:                "Concatenando cadenas"
html_title:           "PowerShell: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

# ¿Qué es y por qué los programadores lo hacen?

La concatenación de cadenas es la acción de combinar varias cadenas de caracteres en una sola. Los programadores lo hacen para crear cadenas más complejas con un único valor, lo que les permite manipular y mostrar datos de forma más eficiente.

## ¿Cómo hacerlo?

Para concatenar cadenas en PowerShell, se utiliza el operador `+` entre las cadenas que se quieren fusionar. Por ejemplo:

```PowerShell
# Definir cadenas
$cadena1 = "Hola"
$cadena2 = "mundo"

# Concatenación de cadenas
$cadena3 = $cadena1 + $cadena2

# Mostrar el resultado
$cadena3 # Salida: "Hola mundo"
```

También es posible utilizar una interpolación de cadenas, que es una forma más sencilla de concatenar cadenas en PowerShell. Para ello, se utiliza el símbolo `$` antes de las variables que se quieren incluir en la cadena. Por ejemplo:

```PowerShell
# Definir cadenas
$cadena1 = "3"
$cadena2 = "5"

# Concatenación de cadenas
$cadena3 = "La suma de $cadena1 y $cadena2 es $($cadena1 + $cadena2)"

# Mostrar el resultado
$cadena3 # Salida: "La suma de 3 y 5 es 8"
```

## Profundizando

La concatenación de cadenas ha sido utilizada desde los primeros lenguajes de programación y sigue siendo una técnica común en la actualidad. Además de utilizar el operador `+` o la interpolación de cadenas, también es posible utilizar el método `Concat()` de la clase `String`. Este método es más eficiente al concatenar grandes cantidades de cadenas.

Como alternativas a la concatenación, existen otros métodos para manipular cadenas, como la sustitución de cadenas con el método `Replace()` o la división de cadenas con el método `Split()`.

En términos de implementación, cuando se concatenan cadenas, se asigna un nuevo espacio de memoria para almacenar la cadena resultante, lo que puede ser una preocupación en programas con grandes cantidades de datos.

## Ver también

- [Documentación de Microsoft sobre la concatenación de cadenas en PowerShell](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1#arithmetic-operators)
- [Artículo de Wikipedia sobre la concatenación de cadenas](https://es.wikipedia.org/wiki/Concatenaci%C3%B3n_de_cadenas)