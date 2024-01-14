---
title:                "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# ¿Por qué obtener la fecha actual en Bash?

Existen muchas razones por las cuales uno podría querer obtener la fecha actual en un programa Bash. Algunos ejemplos comunes incluyen el registro de eventos, el cálculo de la duración de una tarea o simplemente tener la fecha actual para mostrar en un mensaje o archivo. Afortunadamente, Bash ofrece una forma sencilla de obtener la fecha actual y trabajar con ella en nuestros programas.

## Cómo obtener la fecha actual en Bash

Para obtener la fecha actual en Bash, podemos utilizar el comando `date`. Este comando nos permite mostrar la fecha y hora actual en diferentes formatos, dependiendo de nuestras necesidades. Veamos algunos ejemplos:

```
# Muestra la fecha y hora actual en formato YYYY-MM-DD HH:MM:SS
date +"%Y-%m-%d %H:%M:%S"

# Muestra la fecha actual en formato DD/MM/YYYY
date +"%d/%m/%Y"

# Muestra la hora actual en formato HH:MM:SS
date +"%H:%M:%S"
```

El comando `date` también nos permite modificar la fecha y hora actual. Por ejemplo, si queremos obtener la fecha y hora de ayer, podemos utilizar el siguiente comando:

```
# Obtiene la fecha y hora de ayer en formato DD/MM/YYYY HH:MM:SS
date -d "1 day ago" +"%d/%m/%Y %H:%M:%S"
```

Además, podemos almacenar la fecha actual en una variable para utilizarla en nuestro programa Bash. Por ejemplo:

```
# Almacena la fecha actual en una variable
fecha=$(date +"%d/%m/%Y")
echo "Hoy es $fecha"
```

## Profundizando en la obtención de la fecha actual en Bash

El comando `date` cuenta con muchas más opciones y formatos de fecha y hora que no hemos mencionado en este artículo. Además, también podemos utilizar otras herramientas como `cal` o `hwclock` para obtener información más detallada sobre la fecha y hora actual en Bash.

También es importante tener en cuenta que la fecha y hora en Bash está influenciada por la configuración regional de nuestro sistema. Esto puede afectar el formato en el que se muestra la fecha y hora, por lo que es importante tenerlo en cuenta al utilizar el comando `date` en nuestro programa.

# Ver también

- [Manual de Bash: date](https://www.gnu.org/software/bash/manual/html_node/The-date-Command.html)
- [Ejemplos avanzados de date en Linux](https://www.tecmint.com/linux-date-command-examples/)