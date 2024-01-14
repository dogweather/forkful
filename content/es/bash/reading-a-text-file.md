---
title:                "Bash: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Bash

¿Alguna vez te has preguntado cómo puedes leer y manipular un archivo de texto usando Bash? Aprender a hacerlo puede ser increíblemente útil, ya que te permitirá automatizar tareas y procesar información de manera más eficiente. En este artículo, te mostraremos cómo leer un archivo de texto en Bash y profundizaremos en algunos conceptos importantes sobre este tema.

## Cómo hacerlo

Para leer un archivo de texto en Bash, podemos utilizar el comando `cat`. Por ejemplo, si tenemos un archivo llamado "nombres.txt" con una lista de nombres, podemos usar el siguiente comando para imprimir el contenido del archivo en la terminal:

```Bash
cat nombres.txt
```

El resultado sería una lista con los nombres contenidos en el archivo, uno en cada línea. Podemos también almacenar el contenido del archivo en una variable para posteriormente manipularlo. Por ejemplo:

```Bash
nombres=$(cat nombres.txt)
```

Ahora la variable `nombres` contiene una cadena con todos los nombres del archivo de texto. También podemos utilizar el comando `read`, que nos permitirá leer cada línea del archivo de texto y almacenarla en una variable. Por ejemplo:

```Bash
while read linea; do
  echo "El nombre es: $linea"
done < nombres.txt
```

Este código recorre cada línea del archivo y la imprime en la pantalla precedida por "El nombre es:". Ten en cuenta que para poder usar el comando `read`, debemos usar el operador `<` seguido del nombre del archivo que queremos leer.

## Profundizando en la lectura de archivos de texto

Ahora que ya sabemos cómo leer archivos de texto en Bash, es importante recordar algunos aspectos importantes al trabajar con ellos. Por ejemplo, es posible que nuestro archivo contenga espacios en blanco o caracteres especiales que puedan causar errores al leerlo. Para solucionar esto, podemos utilizar el comando `IFS` para especificar un delimitador al leer cada línea del archivo. Por ejemplo, si nuestro archivo está separado por comas, podemos usar lo siguiente:

```Bash
while IFS=',' read -r nombre apellido; do
  echo "El nombre es: $nombre y el apellido es: $apellido"
done < nombres.txt
```

De esta manera, cada línea del archivo será asignada a las variables `nombre` y `apellido`, separadas por la coma especificada en `IFS`.

Otro tema importante a tener en cuenta al leer archivos de texto en Bash es el manejo de errores. Si nuestro archivo no existe o no tiene permisos de lectura, nuestro script puede fallar. Para evitar esto, podemos usar una estructura `if` para verificar si el archivo existe y si podemos leerlo. En caso contrario, podemos mostrar un mensaje de error o finalizar el script.

## Ver también

- [Bash Reference Manual: Redirections](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Bash Guide for Beginners: Reading files](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_08_02.html)