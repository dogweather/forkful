---
title:    "Haskell: Obteniendo la fecha actual"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué 

En el mundo de la programación, a menudo necesitamos obtener y manipular fechas y horas en nuestros programas. Esto puede ser útil en aplicaciones de seguimiento de tiempo, registro de eventos y muchas otras situaciones. Afortunadamente, en Haskell, obtener la fecha actual es una tarea sencilla y hoy te mostraré cómo hacerlo.

## Cómo hacerlo

Para obtener la fecha actual en Haskell, usamos la función `getCurrentTime` del módulo `Data.Time`. Primero, importamos el módulo en nuestro archivo de código:

```
import Data.Time
```

Luego, llamamos a la función `getCurrentTime` y almacenamos el resultado en una variable llamada `fechaActual`:

```
fechaActual <- getCurrentTime
```

¡Eso es todo! Ahora, si imprimimos la variable `fechaActual`, veremos la fecha y hora actual en formato ISO8601:

```
> fechaActual
2019-10-21 09:36:28.639441371 UTC
```

## Inmersión profunda

Ahora que ya sabemos cómo obtener la fecha actual, hagamos una pequeña inmersión profunda y exploremos un poco más. Si solo queremos la fecha sin la hora, podemos usar la función `utctDay` para obtener solo la parte de la fecha de nuestro objeto `fechaActual`:

```
> utctDay fechaActual
2019-10-21
```

También podemos obtener la hora actual en formato UTC utilizando la función `utctDayTime`:

```
> utctDayTime fechaActual
34288.462916265s
```

¡Impresionante! Además, podemos manipular fácilmente esta fecha y hora utilizando funciones como `addUTCTime` y `diffUTCTime`, que nos permiten sumar o restar segundos a nuestra fecha actual.

## Ver también

- [Documentación de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Cómo trabajar con fechas y horas en Haskell](https://www.stackbuilders.com/tutorials/haskell/dates-and-times/)
- [Guía rápida para principiantes de Haskell](https://wiki.haskell.org/Spanish_translation_of_Haskell_for_all#Guía_rápida_para_principiantes_de_Haskell) (en español)