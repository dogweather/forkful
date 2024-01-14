---
title:    "PHP: Escribiendo un archivo de texto"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto en PHP puede ser una forma eficiente de almacenar información y datos en un formato legible y accesible. Además, puede ser una manera conveniente de guardar y compartir datos entre diferentes aplicaciones o sistemas.

## Cómo hacerlo

Para escribir un archivo de texto en PHP, se puede utilizar la función `fopen()` para abrir el archivo y especificar si se quiere escribir (`w`) o añadir (`a`) a él. Luego, se puede utilizar la función `fwrite()` para escribir en el archivo. A continuación, se muestra un ejemplo de código:

```PHP
$file = fopen("ejemplo.txt", "w");
fwrite($file, "Este es un ejemplo de texto que se escribirá en el archivo.");
fclose($file);
```

Este código abrirá un archivo llamado "ejemplo.txt" y escribirá el texto especificado en él. Al utilizar la función `fclose()` se asegura de que el archivo se cierre correctamente después de terminar de escribir.

## Profundizando en la escritura de archivos de texto

Existen diferentes métodos y funciones para escribir archivos de texto en PHP, como por ejemplo la función `file_put_contents()` que permite escribir en un archivo en una sola línea de código. También se pueden utilizar loops para escribir en archivos grandes o escribir en formatos específicos, como CSV.

Además, es importante tener en cuenta que al escribir en archivos de texto, es necesario asegurarse de tener los permisos de escritura adecuados para el archivo y comprobar la existencia del mismo antes de intentar escribir en él. También se pueden utilizar excepciones o condicionales para manejar posibles errores al escribir en el archivo.

## Ver también

- [Función fopen en PHP](https://www.php.net/manual/es/function.fopen.php)
- [Función fwrite en PHP](https://www.php.net/manual/es/function.fwrite.php)
- [Función file_put_contents en PHP](https://www.php.net/manual/es/function.file-put-contents.php)