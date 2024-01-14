---
title:    "PHP: Borrando caracteres que coinciden con un patrón"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón es una tarea útil en programación, ya que nos permite manipular y limpiar datos de forma eficiente.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en PHP, podemos utilizar la función `preg_replace()` de la librería de expresiones regulares de PHP. Esta función toma tres argumentos: el patrón que queremos buscar, el texto en el que queremos buscar y opcionalmente un tercer argumento con el texto que queremos insertar en lugar de los caracteres eliminados. Veamos un ejemplo:

```
<?php
$texto = "Mis amigos son Juan y Juana";
$patron = "/J[ou]an/"; // Buscará tanto "Juan" como "Juana"
$nuevo_texto = preg_replace($patron, "", $texto);
echo $nuevo_texto;
// Output: Mis amigos son y
?>
```

En este ejemplo, hemos eliminado tanto "Juan" como "Juana" del texto original y el resultado es "Mis amigos son y". Además, podemos utilizar expresiones regulares más complejas para eliminar patrones específicos de caracteres o incluso condicionar la eliminación a ciertas variables.

## Profundizando

La función `preg_replace()` utiliza la sintaxis de expresiones regulares de Perl, lo que nos da una amplia gama de posibilidades para manipular nuestros datos. Podemos utilizar diferentes metacaracteres para buscar patrones como el punto (.) que representa cualquier carácter, el asterisco (*) que representa 0 o más repeticiones o el signo más (+) que representa 1 o más repeticiones. Además, también podemos utilizar modificadores para realizar búsquedas sensibles a mayúsculas/minúsculas o para modificar la forma en que se realiza la búsqueda.

## Ver también

[Documentación de la función `preg_replace()` en PHP](https://www.php.net/manual/es/function.preg-replace.php).

[Explicación básica de expresiones regulares en español](https://es.wikipedia.org/wiki/Expresi%C3%B3n_regular).