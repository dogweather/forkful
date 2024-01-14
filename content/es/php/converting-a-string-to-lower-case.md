---
title:    "PHP: Convirtiendo una cadena a minúsculas"
keywords: ["PHP"]
---

{{< edit_this_page >}}

### Por qué
¿Alguna vez has tenido que manipular una cadena de texto en tus proyectos de programación? Una de las tareas más comunes es convertir una cadena a minúsculas. Esto se puede hacer fácilmente con PHP, y en este artículo te mostraré cómo hacerlo.

### Cómo hacerlo
Para convertir una cadena a minúsculas en PHP, utilizamos la función `strtolower()`. Esta función toma una cadena como argumento y devuelve una nueva cadena con todos los caracteres en minúscula.

```PHP
$string = "PROGRAMACIÓN EN PHP";
echo strtolower($string); // Salida: programación en php
```

También podemos usar la función `mb_strtolower()` para manejar correctamente los caracteres multibyte en diferentes idiomas.

```PHP
$string = "ЭКСПЕРИМЕНТЫ С PHP";
echo mb_strtolower($string, 'UTF-8'); // Salida: эксперименты с php
```

Incluso podemos convertir solo la primera letra de la cadena a minúscula utilizando la función `lcfirst()`.

```PHP
$string = "PROGRAMACIÓN EN PHP";
echo lcfirst($string); // Salida: pROGRAMACIÓN EN PHP
```

### Profundizando
Ahora que sabemos cómo convertir una cadena a minúsculas en PHP, es importante entender cómo funciona realmente esta conversión. PHP utiliza el conjunto de caracteres ASCII para trabajar con cadenas. Los caracteres en minúsculas se encuentran dentro del rango de 97 a 122 en el conjunto de caracteres ASCII. Entonces, lo que hace la función `strtolower()` es restar 32 a cada código de carácter en la cadena para convertirlos a minúsculas.

### Ver también
- [Funciones de cadena en PHP](https://www.php.net/manual/es/ref.srtring.php)
- [Codificación de caracteres en PHP](https://www.php.net/manual/es/function.mb-srtring.php)
- [Conjunto de caracteres ASCII](https://es.wikipedia.org/wiki/Ascii)