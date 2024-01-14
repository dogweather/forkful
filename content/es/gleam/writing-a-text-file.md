---
title:    "Gleam: Escribiendo un archivo de texto"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto es una habilidad esencial para cualquier programador. Puede ser utilizado para almacenar información, crear scripts o incluso como una forma de documentar el código. Además, el formato de archivo de texto es universalmente reconocido y puede ser abierto en cualquier dispositivo o sistema operativo.

## Cómo escribir un archivo de texto en Gleam

Gleam es un lenguaje de programación funcional que permite escribir de manera fácil y eficiente archivos de texto. A continuación, presentamos un ejemplo de código que muestra cómo crear un archivo de texto llamado "ejemplo.txt" y escribir algunos datos en él.

```Gleam
fn main() {
  let output_file = File.create("ejemplo.txt") //crea el archivo de texto
  File.write(output_file, "¡Hola, mundo!") //escribe en el archivo
  File.append(output_file, " Esto es un ejemplo de archivo de texto en Gleam.") //añade más texto al archivo
}
```

Una vez que se ejecute este código, se creará un archivo de texto llamado "ejemplo.txt" en el directorio donde se esté ejecutando el programa. Si abres el archivo, verás que contiene el texto "¡Hola, mundo! Esto es un ejemplo de archivo de texto en Gleam." 

## Profundizando en la escritura de archivos de texto

Gleam ofrece múltiples funciones y métodos para la escritura de archivos de texto, como por ejemplo: File.read(), que permite leer el contenido de un archivo de texto, File.rename(), que permite cambiar el nombre de un archivo, y File.delete(), que elimina un archivo. Además, se pueden utilizar otros tipos de datos, como listas o mapas, para escribir información más compleja en un archivo de texto.

## Ver también

- [Documentación oficial de Gleam sobre manejo de archivos](https://gleam.run/documentation/manipulating-files)
- [Tutorial de Gleam sobre escritura y lectura de archivos](https://gleam.run/documentation/manipulating-files/tutorial.html)
- [Página de GitHub de Gleam, donde se pueden encontrar más ejemplos de código](https://github.com/gleam-lang/gleam)