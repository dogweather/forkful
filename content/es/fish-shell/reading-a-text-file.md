---
title:    "Fish Shell: Leyendo un archivo de texto."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Fish Shell?

Hay muchas razones por las cuales puede ser útil leer un archivo de texto en Fish Shell. Por ejemplo, puedes querer extraer información específica de un archivo de configuración o de un archivo de registro. También puedes utilizar esta técnica para realizar tareas de mantenimiento en el sistema o para automatizar ciertas tareas. En este artículo, te mostraremos cómo leer un archivo de texto en Fish Shell de manera fácil y eficiente.

## Cómo hacerlo

Para leer un archivo de texto en Fish Shell, podemos utilizar el comando ```cat```. Este comando muestra el contenido de un archivo de texto en la terminal. Por ejemplo, si queremos leer el archivo "info.txt", podemos escribir lo siguiente en la terminal:

```Fish Shell
cat info.txt
```

Esto mostrará todo el contenido del archivo "info.txt" en la terminal. También podemos usar el comando ```head``` o ```tail``` para mostrar solo las primeras o últimas líneas del archivo, respectivamente. Por ejemplo:

```Fish Shell
head info.txt
```

Esto mostrará solo las primeras 10 líneas del archivo. Si queremos especificar cuántas líneas mostrar, podemos usar la opción ```-n``` seguido del número de líneas que queremos mostrar. Por ejemplo:

```Fish Shell
head -n 5 info.txt
```

Esto mostrará solo las primeras 5 líneas del archivo. De manera similar, podemos usar la opción ```-n``` con ```tail``` para mostrar las últimas líneas del archivo.

## Profundizando

Cuando leemos un archivo de texto en Fish Shell, podemos usar diferentes comandos para manipular el contenido del archivo. Por ejemplo, podemos usar el comando ```grep``` para buscar una cadena de texto específica en el archivo. También podemos usar otros comandos como ```sed``` o ```awk``` para realizar modificaciones en el contenido del archivo.

Además, también podemos usar el símbolo de redirección ```>``` para escribir el contenido del archivo en un nuevo archivo o ```>>``` para añadir el contenido al final de un archivo existente. Por ejemplo:

```Fish Shell
cat info.txt >> new_info.txt
```

Esto copiará el contenido del archivo "info.txt" y lo añadirá al final del archivo "new_info.txt". 

¡Con estos comandos, puedes leer y manipular fácilmente archivos de texto en Fish Shell!

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [10 comandos útiles de Fish Shell](https://www.ostechnix.com/10-fish-shell-commands-for-linux-beginners/)
- [Tutorial de Fish Shell](https://www.hostinger.es/tutoriales/comando-fish-shell/)