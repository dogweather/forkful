---
title:    "PHP: Leyendo argumentos de línea de comando"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comandos en PHP

Muchos programadores de PHP se preguntan si leer argumentos de línea de comandos es realmente necesario en sus proyectos. La verdad es que esta habilidad puede ser muy útil en situaciones en las que necesitamos interactuar con nuestro programa desde la terminal. En este artículo, te mostraremos por qué es importante aprender a leer argumentos de línea de comandos en PHP y cómo hacerlo de manera eficiente.

## Cómo leer argumentos de línea de comandos en PHP

En PHP, leer argumentos de línea de comandos es bastante sencillo. Podemos acceder a estos argumentos a través de la variable global $argv, que nos devuelve un array con todos los argumentos pasados al programa. Veamos un ejemplo:

```PHP
<?php
// Supongamos que ejecutamos nuestro programa así: php mi_programa.php argumento1 argumento2
// $argv se vería así: ["mi_programa.php", "argumento1", "argumento2"]

// Podemos acceder a los argumentos directamente por su índice en el array
$primer_argumento = $argv[1]; // "argumento1"
$segundo_argumento = $argv[2]; // "argumento2"

// También podemos recorrer todos los argumentos usando un bucle foreach
foreach ($argv as $argumento) {
  echo $argumento . PHP_EOL; // Imprimiría todos los argumentos en líneas separadas
}

// Podemos hacer lo que queramos con estos argumentos, como realizar operaciones o llamar a otras funciones
?>
```

Como podemos ver, es muy sencillo acceder a los argumentos de línea de comandos en PHP y utilizarlos a nuestro favor.

## Profundizando en la lectura de argumentos de línea de comandos en PHP

Aunque ya hemos mostrado cómo leer estos argumentos, hay algunos detalles a tener en cuenta al usarlos en nuestros programas. Por ejemplo, si queremos pasar argumentos que contengan espacios, debemos incluirlos entre comillas al ejecutar el programa (por ejemplo, php mi_programa.php "argumento con espacios"). Además, en caso de que solo necesitemos acceder a un subconjunto de los argumentos, podemos utilizar la función array_slice() para obtener solo los elementos que nos interesen del array $argv.

## Ver también
- [Documentación oficial de PHP sobre reading command line arguments](https://www.php.net/manual/es/features.commandline.args.php)
- [Tutorial de PHP CLI para principiantes](https://www.freecodecamp.org/news/how-to-create-a-php-cli-tutorial-for-beginners/)

Esperamos que este artículo te haya sido útil para entender la importancia de leer argumentos de línea de comandos en PHP y cómo hacerlo de manera efectiva. ¡Practica y experimenta con ellos en tus próximos proyectos!