---
title:    "PHP: Utilizando expresiones regulares"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar expresiones regulares en PHP?

Las expresiones regulares son una herramienta muy útil para los programadores PHP ya que les permiten buscar y manipular patrones de texto de forma eficiente. Con ellas, es posible validar inputs de formularios, extraer información de cadenas de texto y realizar búsquedas avanzadas en bases de datos. Además, son muy versátiles y pueden ser utilizadas en una amplia gama de situaciones, desde simples scripts hasta aplicaciones web complejas.

## Cómo utilizar expresiones regulares en PHP

Para utilizar expresiones regulares en PHP, se utiliza la función `preg_match()` seguida por el patrón que se desea buscar entre dos barras inclinadas (`/`). Por ejemplo, si se quiere validar un correo electrónico, se puede utilizar el siguiente código:

```PHP
<?php
$email = "ejemplo@ejemplo.com";
if (preg_match("/^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$/", $email)) {
  echo "El correo electrónico es válido";
} else {
  echo "El correo electrónico no es válido";
}
?>
```

En este código, el patrón utilizado entre las barras inclinadas se asegura de que el correo electrónico tenga un formato válido, siguiendo las reglas establecidas por el RFC 5322. Si se quiere extraer información específica de una cadena de texto, se puede utilizar la función `preg_match_all()` para buscar coincidencias múltiples.

## Profundizando en el uso de expresiones regulares en PHP

Las expresiones regulares en PHP son muy poderosas y tienen una serie de opciones y metacaracteres que permiten realizar búsquedas más precisas y complejas. Por ejemplo, se pueden utilizar grupos de captura para extraer información específica de una coincidencia, o utilizar los metacaracteres `\b` para buscar palabras completas y no partes de ellas.

Además, se pueden utilizar modificadores de patrón para hacer que las búsquedas sean más sensibles a mayúsculas y minúsculas, o para buscar sólo al principio o al final de una cadena de texto. Es importante tener en cuenta que las expresiones regulares también pueden generar errores si no se utilizan correctamente, por lo que es recomendable tener un buen entendimiento de su funcionamiento antes de utilizarlas en proyectos importantes.

## También te puede interesar

- [Documentación oficial de PHP sobre expresiones regulares](https://www.php.net/manual/es/book.pcre.php)
- [Curso interactivo de expresiones regulares en PHP](https://regexone.com/references/php)
- [Cheat Sheet de expresiones regulares en PHP](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)