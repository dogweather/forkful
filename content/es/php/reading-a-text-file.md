---
title:                "PHP: Leyendo un archivo de texto"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador PHP, seguramente en algún momento te has encontrado con la necesidad de leer un archivo de texto en tu código. Esto puede ser por diversas razones, como extraer información de un archivo de configuración o procesar datos para mostrar en tu sitio web.

## Cómo hacerlo

Afortunadamente, PHP ofrece una función incorporada para la lectura de archivos de texto: la función `file_get_contents()`. Esta función acepta como parámetro la ruta del archivo que deseas leer y devuelve una cadena con el contenido del archivo.

Veamos un ejemplo de cómo usar esta función:

```PHP
$contenido = file_get_contents("archivo.txt");
echo $contenido; // muestra el contenido del archivo en la pantalla
```

Si quieres que el contenido del archivo se muestre en formato HTML, puedes usar la función `nl2br()` para convertir los saltos de línea en etiquetas `<br>`:

```PHP
$contenido = nl2br(file_get_contents("archivo.txt"));
echo $contenido; // muestra el contenido del archivo en formato HTML
```

Otra forma de leer un archivo de texto en PHP es usando la función `fopen()`, que abre el archivo y devuelve un descriptor de archivo que luego puedes usar para leer el contenido línea por línea:

```PHP
$archivo = fopen("archivo.txt", "r"); // abre el archivo en modo lectura
while (!feof($archivo)) { // recorre el archivo hasta el final
    $linea = fgets($archivo); // lee una línea del archivo
    echo $linea; // muestra la línea en la pantalla
}
fclose($archivo); // cierra el archivo
```

## Profundizando

La función `file_get_contents()` es la forma más simple y rápida de leer un archivo de texto en PHP. Sin embargo, si necesitas realizar operaciones más complejas, como buscar una palabra específica en el archivo o leer solo una parte del mismo, entonces la función `fopen()` puede ser una mejor opción.

Además, al usar la función `fopen()` también puedes especificar el modo de apertura del archivo, por ejemplo, para escribir o agregar contenido. Puedes obtener más información sobre los diferentes modos de apertura en la documentación de PHP.

## Ver también

- Documentación oficial de PHP: [Función file_get_contents()](https://www.php.net/manual/es/function.file-get-contents.php)
- Documentación oficial de PHP: [Función fopen()](https://www.php.net/manual/es/function.fopen.php)
- Documentación oficial de PHP: [Modos de apertura de archivos](https://www.php.net/manual/es/function.fopen.php#refsect1-function.fopen-parameters)