---
title:                "PHP: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por qué capitalizar una cadena en PHP

Si eres un programador en PHP, es probable que en algún momento hayas tenido que capitalizar una cadena, es decir, convertir la primera letra de cada palabra en mayúscula. Puede que te hayas preguntado ¿por qué es necesario hacer esto? En este artículo te explicaremos por qué y cómo capitalizar una cadena en PHP.

## Cómo hacerlo

Para capitalizar una cadena en PHP, podemos utilizar la función `ucwords()`, que devuelve la cadena con la primera letra de cada palabra en mayúscula. Por ejemplo:

```PHP
$cadena = "hola mundo";
echo ucwords($cadena); // Salida: Hola Mundo
```

También podemos utilizar la función `ucfirst()`, que solo capitaliza la primera letra de la cadena:

```PHP
$cadena = "hola mundo";
echo ucfirst($cadena); // Salida: Hola mundo
```

## Profundizando

Pero, ¿por qué necesitamos capitalizar una cadena? Hay varias razones por las que puede ser útil hacerlo:

- Presentación: a veces necesitamos mostrar texto en mayúscula para que se vea más presentable, especialmente en títulos o encabezados.
- Formatos de archivo: algunos formatos de archivo, como los archivos CSV (valores separados por comas), requieren que los encabezados de las columnas estén en mayúscula.
- Consistencia: en un proyecto de programación en equipo, es importante seguir una convención de codificación para que todos los miembros del equipo utilicen el mismo estilo de codificación. Es común que se prefiera capitalizar las palabras clave en mayúscula en lugar de en minúscula.

Ahora que sabemos por qué es importante capitalizar una cadena, también debemos tener en cuenta que esto puede ser diferente dependiendo del idioma en el que estemos trabajando. Por ejemplo, en español, las palabras en mayúscula pueden llevar acentos u otras letras especiales, por lo que es importante utilizar funciones específicas para cada idioma.

## Ver también

Aquí tienes algunos recursos donde puedes profundizar más sobre la capitalización de cadenas en PHP:

- [Documentación oficial de PHP sobre ucwords()](https://www.php.net/manual/es/function.ucwords.php)
- [Documentación oficial de PHP sobre ucfirst()](https://www.php.net/manual/es/function.ucfirst.php)
- [Artículo sobre capitalización de cadenas en español en Viva el Lado Oscuro del Código](https://blog.codinghorror.com/why-you-should-use-initial-caps-in-programming/)
- [Pregunta y respuestas sobre capitalización de cadenas en stackoverflow](https://stackoverflow.com/questions/2459524/php-how-to-capitalize-the-first-letter-of-a-string)