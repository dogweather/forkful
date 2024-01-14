---
title:                "Haskell: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-json.md"
---

{{< edit_this_page >}}

# ¿Por Qué Trabajar con JSON?

Existen muchas razones por las que desarrollar en Haskell es una excelente opción para trabajar con JSON. En primer lugar, Haskell es un lenguaje funcional puro y muy potente, lo que lo hace ideal para manejar datos estructurados como JSON. Además, cuenta con una gran cantidad de librerías y herramientas que facilitan el trabajo con este formato de datos.

# Cómo Trabajar con JSON en Haskell

Para trabajar con JSON en Haskell, primero debemos importar la librería `Data.Aeson`. Esta nos proporcionará funciones y tipos de datos necesarios para manipular y transformar JSON.

Primero, debemos tener un objeto JSON válido en forma de texto, por ejemplo:

```Haskell
jsonText = "{\"nombre\": \"María\", \"edad\": 25, \"trabajo\": \"desarrolladora\"}"
```

Luego, podemos usar la función `decode` de la librería `Data.Aeson` para convertir este texto en un valor Haskell. Por ejemplo:

```Haskell
main = do
   let maybeJson = decode jsonText :: Maybe Value
   case maybeJson of
      Just json -> print json
      Nothing -> print "No se pudo convertir el JSON"
```

El resultado sería:

```Haskell
Object (fromList [("nombre",String "María"),("edad",Number 25),("trabajo",String "desarrolladora")])
```

Ahora podemos trabajar con el objeto JSON como si fuera cualquier otro valor Haskell. Por ejemplo, podemos acceder a sus campos utilizando la función `at`:

```Haskell
let maybeNombre = json `at` ["nombre"] :: Maybe Value
case maybeNombre of
    Just (String nombre) -> print nombre
    Nothing -> print "No se pudo obtener el nombre"
```

El resultado sería:

```Haskell
"María"
```

# Una Mergulho Profundo en el Mundo de JSON en Haskell

Si queremos profundizar aún más en el trabajo con JSON en Haskell, podemos explorar otras librerías como `Data.Aeson.Lens` que nos permite acceder a los campos de un objeto JSON de manera más sencilla utilizando lentes funcionales.

También podemos investigar sobre cómo trabajar con JSON de manera asincrónica utilizando la librería `data-aeson-qq` y cómo realizar validaciones de esquema con `Data.Aeson.Schema`.

# Ver También

- [Documentación de Data.Aeson](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html)
- [Tutorial para trabajar con JSON en Haskell](https://www.haskell.org/ghc/docs/6.12.2/html/libraries/json-tutorial/Data-Aeson.html)
- [Ejemplos de uso de Data.Aeson](https://github.com/bos/aeson/tree/master/examples)