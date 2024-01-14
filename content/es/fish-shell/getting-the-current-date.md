---
title:    "Fish Shell: Obteniendo la fecha actual"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Si tienes un programa o script que necesita saber la fecha actual, es esencial que puedas obtener esta información de manera rápida y eficiente. Afortunadamente, Fish Shell ofrece una manera sencilla y eficaz de obtener la fecha actual.

## Cómo hacerlo

Para obtener la fecha actual en Fish Shell, simplemente usa el comando `date` seguido de la opción `+%Y%m%d`. Esto devolverá la fecha en formato `año-mes-día`.

```Fish Shell
date +%Y%m%d
```

La salida se verá así: `20201120` (siéntete libre de probarlo por ti mismo). Si prefieres el formato `día-mes-año`, simplemente cambia la opción a `+%d%m%Y`.

También puedes usar opciones adicionales para incluir la hora y la zona horaria, como `+%H%M%S` para la hora y `+%Z` para la zona horaria. Por ejemplo:

```Fish Shell
date +%Y%m%d%H%M%S%Z
```

La salida se verá así: `20201120125312EST`.

## Profundizando

El comando `date` utiliza la biblioteca del sistema para obtener la fecha y la hora actual. Puedes profundizar en esta biblioteca para obtener aún más información sobre la fecha actual, como el día de la semana o el número de semana del año.

Por ejemplo, para obtener el día de la semana en formato numérico (donde el domingo es `0` y el sábado es `6`), simplemente agrega la opción `+%u` al comando:

```Fish Shell
date +%u
```

La salida se verá así: `5` (refiriéndose al viernes, ya que hoy es viernes mientras escribo esto).

Para obtener el número de semana del año en formato numérico, usa la opción `+%V`:

```Fish Shell
date +%V
```

La salida se verá así: `47`.

Puedes explorar más estas opciones y encontrar la combinación que mejor se adapte a tus necesidades.

## Ver también

- [Documentación de Fish Shell para el comando `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Artículo de Linux Handbook sobre cómo obtener la fecha actual en la terminal](https://linuxhandbook.com/get-current-date-time-shell-script/)
- [Página de Wikipedia sobre el comando `date`](https://en.wikipedia.org/wiki/Date_(Unix))