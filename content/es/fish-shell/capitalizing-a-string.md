---
title:    "Fish Shell: Capitalizando una cadena"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

A veces es necesario que un texto se presente con su primera letra en mayúscula, ya sea para fines estéticos o para cumplir con un formato específico. En este caso, capitalizar una cadena de texto puede ser una solución útil y sencilla de implementar.

## Cómo hacerlo

En el shell de Fish, podemos utilizar el comando `string capitalize` para convertir la primera letra de una cadena en mayúscula. Por ejemplo, si tenemos la cadena "hola mundo", al utilizar el siguiente comando:

```Fish Shell 
echo "hola mundo" | string capitalize
```

El resultado será "Hola mundo". Podemos incluso capitalizar todas las palabras de la cadena utilizando el parámetro `-a`:

```Fish Shell 
echo "esto es una prueba" | string capitalize -a
```

Obteniendo como resultado "Esto Es Una Prueba".

## Profundizando

El comando `string capitalize` también nos permite especificar un delimitador, de manera que sólo las palabras después del delimitador serán capitalizadas. Por ejemplo, si utilizamos el delimitador "-" en la cadena "mi-cadena-de-prueba", obtendremos "mi-Cadena-De-Prueba" como resultado.

Además, es importante tener en cuenta que este comando no afecta a las mayúsculas o minúsculas de las letras que no están siendo capitalizadas. Por ejemplo, en la cadena "el mundo es un lugar maravilloso", el resultado sería "El Mundo Es Un Lugar Maravilloso".

## Ver también

- [Documentación oficial de `string capitalize`](https://fishshell.com/docs/current/cmds/string-capitalize.html)
- [Lista de comandos y parámetros de Fish Shell](https://fishshell.com/docs/current/cmds.html)
- [Introducción a la programación con Fish Shell](https://medium.com/@zamirverse/fish-shell-una-alternativa-moderna-para-el-shell-de-unix-e61bd5b2f191)