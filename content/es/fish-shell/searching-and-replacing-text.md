---
title:    "Fish Shell: Buscando y reemplazando texto"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
Es muy común en la programación tener que buscar y reemplazar rápidamente ciertos textos en nuestros archivos. Ya sea para corregir un error, cambiar un valor o simplemente para actualizar un conjunto de datos, la funcionalidad de búsqueda y reemplazo es una herramienta muy útil y eficiente. Aprender a utilizarla en Fish Shell puede agilizar y mejorar nuestro flujo de trabajo.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo en Fish Shell, utilizaremos el comando `sed`, que viene incluido por defecto en la mayoría de distribuciones de Linux y en Mac OS. Este comando nos permite buscar y reemplazar texto a través de expresiones regulares. Veamos un ejemplo:

```Fish Shell
sed -i 's/hola/adios/g' archivo.txt
```

En este ejemplo, estamos buscando y reemplazando la palabra "hola" por "adios" en el archivo "archivo.txt". La opción `-i` nos permite hacer el reemplazo directamente en el archivo, en lugar de imprimirlo en la pantalla.

También podemos utilizar el comando `sed` para realizar reemplazos en varios archivos a la vez. Por ejemplo:

```Fish Shell
sed -i 's/hola/adios/g' *.txt
```

Este comando buscará y reemplazará "hola" por "adios" en todos los archivos con extensión ".txt" en la carpeta actual.

Otra forma de realizar un reemplazo en Fish Shell es utilizando el operador `=~`, que nos permite utilizar expresiones regulares directamente en la línea de comandos. Veamos un ejemplo:

```Fish Shell
echo "hola mundo" | sed 's/[aeiou]/0/g'
```

En este caso, estamos utilizando el comando `echo` para imprimir la frase "hola mundo" y luego, con `sed`, estamos reemplazando todas las vocales por "0".

## Profundizando
El comando `sed` es realmente poderoso y ofrece muchas más funcionalidades. Podemos utilizarlo para reemplazar un texto específico en una línea específica de un archivo, para reemplazar solo la primera aparición de un texto, para buscar y reemplazar en otro directorio distinto al actual, entre muchas otras opciones. Para conocer más en detalle todas las posibilidades de `sed`, te recomendamos revisar su página de manual con el comando `man sed` o buscar tutoriales dedicados a esta herramienta.

## Ver también
- [Página de manual de sed](https://linux.die.net/man/1/sed)
- [Introducción a búsqueda y reemplazo en Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_search_replace)
- [Tutoriales de expresiones regulares en Fish Shell](https://fishshell.com/docs/current/index.html#regex)