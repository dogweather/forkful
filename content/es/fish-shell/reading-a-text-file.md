---
title:    "Fish Shell: Leyendo un archivo de texto"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Por qué leer un archivo de texto

Si eres nuevo en la programación, es posible que te preguntes por qué deberías dedicar tiempo a aprender a leer archivos de texto. La respuesta es simple: los archivos de texto son una forma común y útil de almacenar información en la mayoría de los lenguajes de programación. Saber cómo leerlos te permitirá trabajar con una amplia gama de datos y realizar tareas importantes en tus programas.

## Cómo hacerlo

La Shell de Pescado (Fish Shell) tiene un comando incorporado para leer archivos de texto: `cat`. Puedes usarlo de la siguiente manera:

```Fish Shell
cat nombre_del_archivo.txt
```

Esto mostrará el contenido del archivo de texto en la terminal. Si quieres guardar esa información en una variable, puedes hacerlo de la siguiente manera:

```Fish Shell
variable=$(cat nombre_del_archivo.txt)
```

Ahora puedes usar la variable en tu programa para realizar operaciones con su contenido.

## Profundizando

Además del `cat`, también puedes usar otros comandos de la Shell de Pescado para leer archivos de texto. Por ejemplo, `head` te permite ver las primeras líneas de un archivo, `tail` te muestra las últimas líneas, y `less` te permite navegar por todo el contenido del archivo.

También es importante tener en cuenta que puedes especificar opciones con estos comandos para realizar acciones específicas. Por ejemplo, puedes usar `head -n 5` para ver solo las primeras 5 líneas del archivo.

## Ver también

- [Tutorial de la Shell de Pescado](https://fishshell.com/docs/current/tutorial.html)
- [Documentación oficial de la Shell de Pescado](https://fishshell.com/docs/current/index.html)
- [Guía de inicio rápido de la Shell de Pescado](https://fishshell.com/docs/current/tutorial.html#tut_quickstart)