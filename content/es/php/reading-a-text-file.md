---
title:                "PHP: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Si eres un programador de PHP, seguramente te has encontrado con la necesidad de leer archivos de texto en tus proyectos. Ya sea para leer datos de configuración o para procesar archivos CSV, saber cómo leer y manipular archivos de texto es una habilidad importante para cualquier programador. En esta publicación, te mostraremos cómo hacerlo de manera sencilla y eficiente.

## Cómo hacerlo

Para leer un archivo de texto en PHP, primero debes abrir el archivo con la función `fopen()`, especificando el modo de apertura como "r" para lectura. A continuación, puedes utilizar la función `fgets()` para leer el contenido del archivo línea por línea. Aquí tienes un ejemplo:

```
$f = fopen("archivo.txt", "r"); // Abrir archivo para lectura
while(!feof($f)) { // Iterar mientras no se haya alcanzado el final del archivo
  $linea = fgets($f); // Leer una línea del archivo
  echo $linea . "<br>"; // Imprimir la línea
}
fclose($f); // Cerrar el archivo
```

Si deseas leer todo el contenido del archivo de una sola vez, puedes utilizar la función `file_get_contents()`:

```
$contenido = file_get_contents("archivo.txt"); // Leer todo el contenido del archivo
echo $contenido; // Imprimir el contenido
```

## Profundizando

Además de las funciones mencionadas anteriormente, PHP ofrece otras opciones para leer archivos de texto. Por ejemplo, la función `file()` permite leer todo el contenido del archivo y almacenarlo en un array, donde cada elemento representa una línea del archivo. También puedes utilizar la función `fread()` para leer una cantidad específica de bytes del archivo, en lugar de una línea completa.

Otra forma de leer archivos de texto es utilizando las expresiones regulares, que te permiten buscar y extraer información específica del archivo. Puedes encontrar muchos recursos en línea para aprender más sobre expresiones regulares y cómo utilizarlas en tus proyectos de PHP.

## Ver también

- [Documentación oficial de PHP para el manejo de archivos](https://www.php.net/manual/es/function.fopen.php)
- [Tutorial de PHP para leer archivos en formato CSV](https://uniwebsidad.com/libros/php-avanzado/capitulo-3/lectura-de-datos-almacenados-en-un-archivo-de-texto)
- [Recursos para aprender expresiones regulares en PHP](https://www.lawebdelprogramador.com/codigo/PHP/4115-Introduccion-a-las-Expresiones-Regulares.html)

¡Esperamos que esta publicación te haya sido útil y te haya ayudado a entender mejor cómo leer archivos de texto en PHP! Recuerda siempre consultar la documentación oficial y continuar mejorando tus habilidades de programación. ¡Hasta la próxima!