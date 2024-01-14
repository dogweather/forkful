---
title:    "Fish Shell: Convirtiendo una cadena a minúsculas"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué convertir una cadena a minúsculas

La tarea de convertir una cadena de texto a minúsculas puede ser muy útil en el desarrollo de aplicaciones web, especialmente cuando se trata de validación de entradas de usuarios. Al hacerlo, podemos asegurarnos de que las comparaciones de cadenas sean precisas y evitar errores de sintaxis.

## Cómo hacerlo

En Fish Shell, esta tarea es muy sencilla gracias al comando `string`:lower. Simplemente debemos proporcionar la cadena a la que deseamos convertir a minúsculas entre comillas, como se muestra en el siguiente ejemplo:

```
Fish Shell> string:lower "HOLA MUNDO"
hola mundo
```

Como podemos ver, el comando `string:lower` ha convertido la cadena "HOLA MUNDO" a minúsculas y nos ha devuelto el resultado en la línea siguiente.

Otra opción es utilizar la función `tolower` de Fish Shell, que realiza la misma tarea pero con una sintaxis diferente. Veámoslo en acción:

```
Fish Shell> tolower "¡¿CÓMO ESTÁS?"
¡¿cómo estás?
```

Ambos métodos son igualmente válidos, por lo que podemos elegir el que nos resulte más cómodo.

## Profundizando

Es importante tener en cuenta que la conversión a minúsculas no solo afecta a las letras, sino también a los caracteres especiales y espacios. Por ejemplo, la cadena "¡Hola Mundo!" se convertiría a "¡hola mundo!".

Además, en algunos casos podríamos encontrarnos con un comportamiento inesperado al convertir ciertas letras a minúsculas debido a las diferencias en la localización y codificación de caracteres. Por ejemplo, la letra "Ñ" en algunos idiomas puede convertirse a "ñ" en lugar de "N". Por ello, es importante probar nuestros comandos con diferentes cadenas de texto para asegurarnos de que obtengamos el resultado deseado.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell en español](https://dev.to/pablokbs/introduccion-a-fish-shell-4gk6)
- [Guía completa de comandos de Fish Shell en español](https://blog.desdelinux.net/guia-comandos-fish-shell/)