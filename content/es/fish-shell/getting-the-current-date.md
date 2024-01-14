---
title:    "Fish Shell: Obteniendo la fecha actual"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Fish Shell?

Obtener la fecha actual puede ser útil en muchos escenarios de programación, ya sea para mostrar la fecha en la que se realizó una determinada acción o para realizar operaciones con fechas en tu código. En esta entrada, aprenderemos cómo obtener la fecha actual en Fish Shell utilizando comandos sencillos.

## Cómo hacerlo

Fish Shell ofrece una variedad de comandos para obtener la fecha actual. El más común es el comando `date`, que muestra la fecha y hora en el formato especificado. Por ejemplo, si ejecutas el siguiente comando en tu terminal:

```Fish Shell
date +%d/%m/%G
```

Obtendrás la fecha actual en formato día/mes/año, por ejemplo: 26/08/2021.

También puedes utilizar el comando `cal` para obtener un calendario del mes actual. Si lo ejecutas sin ningún argumento, mostrará el calendario del mes en curso. Pero también puedes especificar un mes y año en particular para obtener su calendario. Por ejemplo:

```Fish Shell
cal 12 2021
```

Este comando mostrará el calendario del mes de diciembre de 2021.

Otra opción es utilizar el comando `ls -lT`, que muestra la fecha y hora de la última modificación de un archivo. Si lo ejecutas sin ningún argumento, mostrará la fecha y hora de la carpeta actual. Pero también puedes especificar un archivo en particular para obtener su información.

## Profundizando en la obtención de la fecha actual

En realidad, el comando `date` es una abreviatura de `date "+FORMATO"`, lo que significa que puedes personalizar el formato en el que se muestra la fecha actual. Por ejemplo, puedes mostrar la fecha en formato largo utilizando el siguiente comando:

```Fish Shell
date +"%A, %d de %B de %G"
```

Esto mostrará la fecha actual en formato día de la semana, día del mes, mes y año en letras: jueves, 26 de agosto de 2021.

Además, Fish Shell también ofrece una opción interactiva para obtener la fecha actual. Si escribes `alt + d` en tu terminal, se abrirá un pequeño menú donde puedes ver la fecha y hora actuales y elegir un formato para mostrarlos.

## Ver también

- Página de manual de Fish Shell sobre el comando `date`: https://fishshell.com/docs/current/cmds/date.html
- Página de manual de Fish Shell sobre el comando `cal`: https://fishshell.com/docs/current/cmds/cal.html
- Página de manual de Fish Shell sobre el comando `ls`: https://fishshell.com/docs/current/cmds/ls.html