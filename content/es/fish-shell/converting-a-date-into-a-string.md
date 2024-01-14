---
title:    "Fish Shell: Convirtiendo una fecha en un texto"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una tarea común en la programación. Puede ser útil para mostrar fechas en un formato específico o para realizar operaciones matemáticas basadas en fechas. En este artículo, aprenderemos cómo hacerlo utilizando el Fish Shell.

## Cómo

Para convertir una fecha en una cadena de texto en Fish Shell, podemos utilizar el comando `date` seguido del formato deseado. Por ejemplo:

```
Fish Shell $ date +%Y-%m-%d
```

Esto nos devolverá la fecha actual en el formato "año-mes-día".

```
2021-10-18
```

También podemos utilizar variables para almacenar la fecha y luego imprimirlo en un formato específico. Por ejemplo:

```
set now (date +%d/%m/%Y)
echo $now
```

Esto nos devolverá la fecha actual en el formato "día/mes/año".

```
18/10/2021
```

Para obtener más información sobre los formatos admitidos y cómo utilizarlos, podemos consultar la documentación del comando `date` en Fish Shell.

## Deep Dive

En Fish Shell, el comando `date` utiliza la biblioteca GNU Coreutils para manipular fechas y tiempos. Esta biblioteca es muy potente y nos permite realizar operaciones avanzadas, como cambiar la zona horaria, convertir entre formatos de fecha, y más.

Por ejemplo, si queremos obtener la fecha de hace una semana en un formato corto, podemos utilizar la opción `-d` para especificar la fecha y `-v-1w` para restar una semana.

```
Fish Shell $ date -d "1 week ago" +%d/%m/%Y
```

Esto nos devolverá la fecha de hace una semana en el formato "día/mes/año".

```
11/10/2021
```

## Ver también

- [Documentación de `date` en Fish Shell](https://fishshell.com/docs/current/commands.html#date)
- [Manual de GNU Coreutils](https://www.gnu.org/software/coreutils/manual/coreutils.html)