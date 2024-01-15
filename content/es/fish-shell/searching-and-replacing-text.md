---
title:                "Buscar y reemplazar texto"
html_title:           "Fish Shell: Buscar y reemplazar texto"
simple_title:         "Buscar y reemplazar texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué?
Si alguna vez has tenido que cambiar una palabra o frase repetida en un documento o archivo, sabrás lo tedioso que puede ser hacerlo manualmente. Con la ayuda de la función de búsqueda y reemplazo en Fish Shell, puedes realizar esta tarea de manera rápida y eficiente.

## Cómo hacerlo
Primero, debemos abrir nuestra terminal y escribir "fish" para ingresar al shell. Luego, utilizaremos el comando `sed` junto con la sintaxis `s/buscar/reemplazar/g` para buscar y reemplazar una palabra o frase en un archivo específico. Por ejemplo, si queremos reemplazar todas las apariciones de la palabra "hola" por "adiós" en un archivo llamado "saludos.txt", el comando sería:

```Fish Shell
sed -i 's/hola/adiós/g' saludos.txt
```

El argumento `-i` indica que los cambios se harán directamente en el archivo y no se crearán copias. También se puede utilizar la sintaxis `s/buscar/reemplazar/` sin el argumento `g` para reemplazar solo la primera aparición de la palabra en cada línea.

## Profundizando
Fish Shell también ofrece opciones adicionales para la función de búsqueda y reemplazo. Por ejemplo, podemos utilizar el símbolo `~` después del comando para imprimir el resultado de cada cambio realizado. Además, también podemos utilizar expresiones regulares en nuestro patrón de búsqueda para ser más específicos en los cambios que queremos realizar.

## Ver también
- [Documentación oficial de Fish Shell sobre la función de búsqueda y reemplazo](https://fishshell.com/docs/current/cmds/sed.html)
- [Tutorial de uso de expresiones regulares en Fish Shell](https://medium.com/@eferhatg/fish-shell-regex-tutorial-2e936e5683a7)
- [Lista de comandos útiles de sed en Fish Shell](https://www.redhat.com/sysadmin/sed-command-cheatsheet)