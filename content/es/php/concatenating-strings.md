---
title:                "PHP: Uniendo cadenas de texto"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Una de las tareas más comunes en programación es la manipulación de cadenas de texto. Ya sea para mostrar información al usuario, almacenar datos en una base de datos o realizar cálculos, las cadenas de texto son una parte esencial de cualquier programa. Una técnica útil para manejar estas cadenas es la concatenación de strings.

## How To

En PHP, la concatenación de strings se realiza mediante el operador "." (punto). Este operador permite unir dos o más cadenas de texto en una sola. Veamos un ejemplo:

```PHP
$nombre = "María";
$apellido = "García";

echo $nombre . $apellido;

// Output: MaríaGarcía
```

En este caso, el operador "." une las variables $nombre y $apellido en una sola cadena de texto, sin agregar ningún espacio entre ellas. Sin embargo, si deseamos agregar un espacio, podemos hacerlo de la siguiente manera:

```PHP
echo $nombre . " " . $apellido;

// Output: María García
```

También es posible concatenar tres o más cadenas de texto utilizando el operador de la misma manera:

```PHP
$titulo = "Bienvenido";
$mensaje = "al blog de programación";
$autor = "Juan";

echo $titulo . " " . $mensaje . " " . $autor;

// Output: Bienvenido al blog de programación Juan
```

Además de unir cadenas de texto, el operador "." también puede ser utilizado para concatenar variables con texto estático o con el resultado de una función. Veamos un ejemplo:

```PHP
$precio = 25;
$impuesto = 0.16;

echo "El precio final es: $" . ($precio + ($precio * $impuesto));

// Output: El precio final es: $29
```

## Deep Dive

En el ejemplo anterior, utilizamos paréntesis para indicarle a PHP que primero realice el cálculo de la suma antes de concatenar el resultado con el texto estático. Esto se debe a que PHP evalúa las expresiones de izquierda a derecha.

Otra forma de concatenar cadenas de texto en PHP es utilizando la función "concat". Esta función toma como parámetros dos o más cadenas de texto y las une en una sola. Veamos un ejemplo:

```PHP
$nombre = "Ana";
$apellido = "Pérez";

echo concat($nombre, $apellido);

// Output: AnaPérez
```

## See Also

- [Documentación oficial de PHP sobre concatenación de strings](https://www.php.net/manual/es/language.operators.string.php)
- [Tutorial de concatenación de strings en PHP](https://www.w3schools.com/php/php_operators.asp)