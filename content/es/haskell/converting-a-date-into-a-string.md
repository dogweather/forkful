---
title:    "Haskell: Convirtiendo una fecha en una cadena"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Una de las tareas más comunes en la programación es la conversión de datos de un formato a otro. En el caso específico de las fechas, a veces es necesario convertirlas a formato de cadena de texto para poder manipularlas o mostrarlas de manera adecuada. En este artículo, te mostraré cómo convertir una fecha en formato de cadena de texto en tu programa Haskell.

## Cómo hacerlo

Para convertir una fecha en formato de cadena de texto, utilizaremos la función `formatTime` de la librería `Data.Time.Format`. Esta función toma como parámetros una cadena de formato y un valor de tipo `UTCTime` (tiempo universal coordinado) y devuelve la fecha en formato de cadena.

Por ejemplo, si queremos mostrar la fecha actual en formato DD/MM/AAAA, podemos hacerlo de la siguiente manera:

```Haskell
import Data.Time.Format

fecha_actual <- getCurrentTime
formatTime defaultTimeLocale "%d/%m/%Y" fecha_actual
```

El resultado será "04/07/2021", dependiendo de la fecha actual en la que se ejecute el código.

## Inmersión profunda

La cadena de formato que se pasa como primer parámetro a `formatTime` sigue una sintaxis específica para indicar cómo se debe mostrar la fecha. Algunos de los caracteres más comunes son:

- `d`: día del mes en formato numérico (sin ceros a la izquierda)
- `dd`: día del mes en formato numérico (con ceros a la izquierda)
- `m`: mes en formato numérico (sin ceros a la izquierda)
- `mm`: mes en formato numérico (con ceros a la izquierda)
- `y`: año en formato numérico (sin ceros a la izquierda)
- `yy`: año en formato numérico (2 dígitos)
- `yyy`: año en formato numérico (4 dígitos)

También se pueden agregar otros caracteres como barras (/), guiones (-) o puntos (.) para separar las partes de la fecha.

Es importante tener en cuenta que el formato de la fecha dependerá del local en el que se esté ejecutando el programa. Por ejemplo, en español es común usar la fecha en formato DD/MM/AAAA, mientras que en Estados Unidos se utiliza MM/DD/AAAA. Para especificar un local específico, se puede usar la función `setLocale`.

Puedes encontrar más información sobre la sintaxis de la cadena de formato de fecha en la documentación de Haskell.

## Ver también

- [Documentación de la librería `Data.Time.Format`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Tutorial de Haskell en español](https://www.haskell-es.com/)