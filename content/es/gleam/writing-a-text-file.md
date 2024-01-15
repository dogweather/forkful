---
title:                "Escribiendo un archivo de texto"
html_title:           "Gleam: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto es una forma sencilla y eficiente de almacenar información y datos en un formato legible por humanos. Puede ser una parte esencial en el desarrollo de un programa, ya que permite guardar y acceder a información necesaria para su funcionamiento.

# Cómo hacerlo

Para escribir un archivo de texto en Gleam, necesitaremos utilizar la librería `gleam/io`, la cual nos proporciona funciones para manejar archivos. A continuación, veremos un ejemplo simple de cómo crear un archivo de texto y escribir en él:

```Gleam
import gleam/io

pub fn main() {
  // Creamos un archivo llamado "mi_archivo.txt"
  let file = io.write_file("mi_archivo.txt");

  // Escribimos "¡Hola, mundo!" en el archivo
  io.write(file, "¡Hola, mundo!");
}
```

Podemos ver que primero importamos la librería `gleam/io` y luego utilizamos la función `write_file` para crear un archivo. Luego, utilizando la función `write`, escribimos la cadena de texto "¡Hola, mundo!" en el archivo.

El resultado de este ejemplo sería un archivo de texto llamado "mi_archivo.txt" con el contenido "¡Hola, mundo!".

# Profundizando

Escribir un archivo de texto no se limita solo a escribir cadenas de texto simples. Podemos utilizar otras funciones y tipos de datos para crear un archivo más complejo. Por ejemplo:

```Gleam
import gleam/io

type Person(name: String, age: Int)

pub fn main() {
  // Creamos un archivo llamado "personas.txt"
  let file = io.write_file("personas.txt");

  // Creamos dos personas
  let john = Person("John", 25)
  let sarah = Person("Sarah", 32)

  // Escribimos sus datos en el archivo
  io.write(file, john.name ++ " tiene " ++ john.age ++ " años.")
  io.write(file, sarah.name ++ " tiene " ++ sarah.age ++ " años.")
}
```

En este ejemplo, creamos un tipo de datos `Person` que representa a una persona con un nombre y una edad. Luego, utilizamos ese tipo de datos para crear dos personas y escribimos su información en el archivo. Al final, el archivo "personas.txt" tendría el contenido "John tiene 25 años. Sarah tiene 32 años."

# Ver también

- Documentación oficial de `gleam/io`: enlace aquí
- Tutorial de Gleam para principiantes: enlace aquí