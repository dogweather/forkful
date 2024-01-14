---
title:    "Python: Escribiendo un archivo de texto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una habilidad esencial en programación ya que permite almacenar y manipular datos de forma sencilla y eficiente. Ya sea para guardar información, generar informes o simplemente para aprender una nueva técnica, saber cómo escribir y leer archivos de texto en Python es importante para cualquier programador.

## Cómo hacerlo

Para escribir un archivo de texto en Python, primero debemos abrirlo en modo escritura utilizando la función `open()` y especificando el nombre del archivo y el modo como parámetros. Utilizaremos el modo `"w"` para escritura, que sobrescribirá cualquier archivo existente o creará uno nuevo si no existe.

```Python
archivo = open("mi_archivo.txt", "w")
```

Luego, podemos utilizar el método `write()` para escribir en el archivo. Este método recibe como parámetro el contenido que queremos escribir, y se puede llamar múltiples veces para añadir más texto.

```Python
archivo.write("Este es un ejemplo de un archivo de texto escrito en Python.\n")
archivo.write("Puedo escribir múltiples líneas y utilizar caracteres especiales como \\n para saltos de línea.\n")
```

Finalmente, para asegurarnos de que los cambios se guarden, debemos cerrar el archivo utilizando el método `close()`.

```Python
archivo.close()
```

Para leer un archivo de texto en Python, utilizamos el mismo método `open()`, pero con el modo `"r"` para lectura. Luego, podemos utilizar el método `read()` para obtener todo el contenido del archivo o el método `readline()` para leer línea por línea.

```Python
archivo = open("mi_archivo.txt", "r")
contenido = archivo.read()
print(contenido)
archivo.close()
```

## Profundizando

Existen otras opciones y métodos para escribir y leer archivos de texto en Python, como utilizar el módulo `os` para manipular archivos y directorios, o utilizar el modo `"a"` para agregar datos al final de un archivo existente. También es importante tener en cuenta la codificación de caracteres al escribir y leer archivos para evitar posibles errores.

## Ver también

- La documentación oficial de Python sobre [manipulación de archivos](https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files)
- Un tutorial sobre [lectura y escritura de archivos en Python](https://realpython.com/read-write-files-python/) de Real Python en inglés.