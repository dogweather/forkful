---
title:    "Haskell: Convirtiendo una fecha en cadena"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

¿Por qué convertir una fecha en una cadena de texto?

A menudo, al trabajar con datos de fechas en un programa, es necesario convertirlas en una cadena de texto para una mejor visualización o para realizar cálculos basados en la fecha. En Haskell, hay varias formas de hacer esta conversión y en este artículo exploraremos algunas de ellas.

## Cómo hacerlo

Para comenzar, importaremos el módulo `Data.Time.Format` que nos proporciona funciones para trabajar con fechas.

```Haskell
import Data.Time.Format
```

Ahora, supongamos que queremos convertir la fecha actual en una cadena de texto con el formato "dd/mm/aaaa". Podemos hacerlo de la siguiente manera:

```Haskell
getCurrentDate :: IO String
getCurrentDate = do
  now <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%d/%m/%Y" now
  return formattedDate
```

Aquí, hemos utilizado la función `getCurrentTime` para obtener la fecha y hora actual en formato `UTCTime` y luego hemos utilizado la función `formatTime` para convertir esa fecha en una cadena de texto con el formato deseado.

Si queremos obtener la fecha en un formato diferente, como "mm/dd/aaaa", solo necesitamos cambiar el parámetro de formato en la función `formatTime`.

```Haskell
getCurrentDate :: IO String
getCurrentDate = do
  now <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%m/%d/%Y" now
  return formattedDate
```

También podemos convertir una fecha específica en una cadena de texto haciendo uso de la función `parseTimeM`. Por ejemplo, si queremos convertir la fecha "7 de julio de 2021" en una cadena de texto con el formato "dd de mmmm de aaaa", podemos hacerlo de la siguiente manera:

```Haskell
getSpecificDate :: IO (Maybe String)
getSpecificDate = do
  let inputDate = "07/07/2021"
      format = "%d de %B de %Y"
  parsedDate <- parseTimeM False defaultTimeLocale format inputDate :: IO (Maybe Day)
  case parsedDate of
    Just date -> return $ Just $ formatTime defaultTimeLocale format date
    Nothing -> return Nothing
```

En este ejemplo, primero definimos la fecha en formato de cadena de texto y luego especificamos el formato en el que queremos que se muestre la fecha. Luego usamos la función `parseTimeM` para convertir la cadena en una fecha y la función `formatTime` para convertirla en una cadena de texto con el formato especificado. Cabe mencionar que la función `parseTimeM` devuelve un `Maybe`, ya que puede ocurrir un fallo al intentar convertir la cadena en una fecha. Por eso, hemos utilizado un `case` para manejar tanto el éxito como el fallo en la conversión.

## Profundizando

En los ejemplos anteriores, hemos utilizado los formatos de fecha y hora predeterminados que proporciona el módulo `Data.Time.Format`. Sin embargo, también es posible definir nuestros propios formatos utilizando la función `timeLocale` y pasarlos como parámetro a `formatTime` o `parseTimeM`.

Además, el módulo `Data.Time.Format` también ofrece funciones para trabajar con diferentes zonas horarias y formatos específicos de idiomas. Con un poco de investigación, puedes encontrar la función adecuada para tus necesidades específicas.

## Ver también

- [Documentación del módulo `Data.Time.Format`](https://haskell.org/haskellwiki/Data.Time.Format).
- [Módulo `Data.Time` para trabajar con diferentes formatos de fechas](https://hackage.haskell.org/package/time).
- [Tutorial sobre el módulo `Data.Time` en Haskell](https://en.wikibooks.org/wiki/Haskell/Datetime).