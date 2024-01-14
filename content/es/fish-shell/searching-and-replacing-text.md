---
title:                "Fish Shell: Buscando y reemplazando texto"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar y reemplazar texto en Fish Shell?

La búsqueda y reemplazo de texto es una tarea común en la programación. Puede ser útil cuando se quiere cambiar una parte específica de un código o cuando se necesita realizar cambios en un gran número de archivos. Con Fish Shell, este proceso se puede realizar de manera rápida y eficiente.

## Cómo hacerlo: Ejemplos de código y resultados

```Fish Shell
# Reemplazar "hola" por "adiós" en un solo archivo
sed -i 's/hola/adiós/' archivo.txt 

# Reemplazar "nombre: Juan" por "nombre: Pedro" en todos los archivos del directorio actual
sed -i 's/nombre: Juan/nombre: Pedro/g' * 

# Reemplazar "border: 1px" por "border: none" en todos los archivos que contienen la palabra "estilo"
sed -i .bak '/estilo/s/border: 1px/border: none/g' * 
```

Todos estos comandos utilizan el comando `sed` para realizar la búsqueda y reemplazo de texto en un solo archivo, en todos los archivos de un directorio o en archivos que contengan cierta palabra. El uso de la opción `-i` permite realizar los cambios directamente en los archivos, sin necesidad de crear un archivo temporal. También se pueden utilizar expresiones regulares para buscar patrones específicos en lugar de texto exacto.

## Un vistazo más profundo

La herramienta `sed` se basa en expresiones regulares, que son patrones utilizados para buscar y manipular texto. Al utilizar el argumento `s` seguido por dos patrones separados por una barra, se puede indicar qué texto buscar y qué texto reemplazar.

Además de `sed`, también se pueden utilizar otras herramientas de búsqueda y reemplazo de texto en Fish Shell, como `awk` y `perl`.

## Ver también

- [Documentación oficial de Fish Shell sobre búsqueda y reemplazo de texto](https://fishshell.com/docs/current/cmds/sed.html)
- [Tutorial de búsqueda y reemplazo de texto en Fish Shell](https://medium.com/@shtev/efficiently-search-and-replace-text-in-fish-shell-linux-5a23c17ec7a3)
- [Guía de expresiones regulares en Fish Shell](https://github.com/fish-shell/fish-shell/wiki/Tutorial-Regex)