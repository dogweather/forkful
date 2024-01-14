---
title:                "Bash: Comprobando si un directorio existe"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador de Bash, es probable que te preguntes si una carpeta específica existe o no antes de realizar ciertas acciones en tu código. Puede ser tedioso y poco efectivo continuar con el resto del código si la carpeta no existe. Por eso, es importante saber cómo verificar la existencia de una carpeta en Bash.

## Cómo hacerlo
Verificar si una carpeta existe en Bash es muy sencillo. Simplemente debemos utilizar el comando `test` con la opción `-d` seguida del nombre de la carpeta que queremos verificar. Por ejemplo:

```Bash
test -d mi_carpeta
```

En este caso, si la carpeta "mi_carpeta" existe, el comando devolverá el valor verdadero (0) y si no existe, devolverá el valor falso (1). Podemos utilizar esta lógica en nuestro código Bash para tomar decisiones basadas en la existencia de la carpeta.

También podemos hacer uso del comando `if` junto con el comando `test` para realizar una acción determinada si la carpeta existe. Por ejemplo:

```Bash
if test -d mi_carpeta; then
  echo "La carpeta existe"
fi
```

En este caso, el código dentro del `if` solo se ejecutará si la carpeta existe.

## Profundizando
Como mencionamos anteriormente, el comando `test` con la opción `-d` es la forma más común de verificar la existencia de una carpeta en Bash. Sin embargo, existen otras opciones que pueden ser útiles en distintas situaciones:

- `-e` nos permite verificar si un archivo o directorio existe.
- `-L` nos permite verificar si un enlace simbólico existe.
- `-s` nos permite verificar si un archivo o directorio tiene un tamaño mayor a cero bytes.

Además, si queremos realizar una acción si la carpeta no existe, podemos utilizar el comando `!` antes de `test`. Por ejemplo:

```Bash
if ! test -d mi_carpeta; then
  mkdir mi_carpeta
fi
```

En este caso, si la carpeta "mi_carpeta" no existe, se creará automáticamente.

## Ver también
Para más información sobre el comando `test`, puedes revisar la documentación oficial: https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html

También puedes aprender más sobre otros comandos útiles en Bash en nuestra guía de comandos básicos: https://linuxconfig.org/bash-scripting-tutorial-for-beginners