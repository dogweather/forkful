---
title:                "Creando un archivo temporal"
html_title:           "Fish Shell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

¿Qué es y por qué se crea un archivo temporal?

Crear un archivo temporal es un proceso en el cual un programador crea un archivo que se utiliza temporalmente para almacenar datos o realizar un proceso específico en un programa. Esta práctica es comúnmente utilizada por programadores para evitar sobrecargar o alterar los archivos permanentes del sistema. 

Cómo hacerlo:

Fish Shell ofrece una forma sencilla de crear archivos temporales utilizando el comando `mktemp`. Este comando permite crear archivos con nombres únicos en una ubicación específica. Aquí tienes un ejemplo de cómo crear un archivo temporal en el directorio actual:

```
Fish Shell > mktemp
/tmp/tmp.RBrZdNNZG7
```
En este ejemplo, el archivo temporal se llama "tmp.RBrZdNNZG7" y se encuentra en el directorio `/tmp/`.

Profundizando:

La práctica de crear archivos temporales ha sido utilizada por programadores durante mucho tiempo. En el pasado, se solía hacer de forma manual, creando un archivo vacío y dándole un nombre único. Actualmente, existen otras alternativas para crear archivos temporales, como el uso de ramas temporales en sistemas de control de versiones como Git.

Es importante destacar que los archivos temporales deben ser eliminados una vez que ya no se necesitan para evitar posibles problemas de seguridad o llenar el espacio de almacenamiento con archivos innecesarios. Por ello, Fish Shell incluye el comando `rm` para eliminar los archivos temporales creados con `mktemp` de la siguiente manera:

```
Fish Shell > rm /tmp/tmp.RBrZdNNZG7
```

Consulta también:

- La documentación oficial de Fish Shell para conocer más sobre el comando `mktemp`: https://fishshell.com/docs/current/cmds/mktemp.html
- Descubre cómo utilizar ramas temporales en Git para trabajar con archivos temporales de forma más eficiente: https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell#_branching