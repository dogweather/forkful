---
title:                "Elm: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON en Elm

En la programación moderna, trabajar con datos es una parte fundamental de cualquier proyecto. Y en Elm, JSON es la forma más común de intercambiar y manipular datos. Aprender a trabajar con JSON en Elm puede mejorar significativamente tus habilidades de programación y ampliar tus posibilidades de creación de aplicaciones.

## Cómo hacerlo

Para empezar, es importante entender cómo Elm maneja los datos en general. En Elm, los datos se representan en forma de árbol, lo que permite una forma eficiente y segura de manipularlos. Para trabajar con JSON, tenemos que convertirlo a un valor de Elm. Por ejemplo, si tenemos el siguiente JSON:

```Elm
{
    "nombre": "Sofía",
    "edad": 28,
    "hobbies": ["leer", "cocinar", "viajar"]
}
```

Podemos convertirlo a un valor de Elm de la siguiente manera:

```Elm
usuario =
    {
        nombre = "Sofía",
        edad = 28,
        hobbies = ["leer", "cocinar", "viajar"]
    }
```

Ahora podemos acceder a los valores del usuario de la siguiente manera:

```Elm
nombre = usuario.nombre
edad = usuario.edad
hobbie1 = List.head usuario.hobbies
```

También podemos actualizar los valores del usuario y convertirlos de nuevo a JSON utilizando la función `encode`:

```Elm
usuarioModificado = 
    {
        usuario | edad = 29
    }
nuevoJSON = Json.Encode.encode 4 usuarioModificado
```

## Profundizando

Trabajar con JSON en Elm no se limita solo a convertir y manipular datos. Elm también tiene funciones útiles para validar y decodificar JSON. Por ejemplo, si tenemos un JSON que representa una lista de usuarios, podemos validar y decodificarlo de la siguiente manera:

```Elm
usuariosJSON = 
    """
    [{
        "nombre": "Sofía",
        "edad": 28
    },
    {
        "nombre": "Juan",
        "edad": 32
    }]
    """

type alias Usuario = 
    {
        nombre : String,
        edad : Int
    }
    
usuariosDecodificados = 
    let 
        decodificador = Json.Decode.list (Json.Decode.map2 Usuario 
            (Json.Decode.field "nombre" Json.Decode.string) 
            (Json.Decode.field "edad" Json.Decode.int))
    in Json.Decode.decodeString decodificador usuariosJSON
```

En el ejemplo anterior, utilizamos el decodificador `list` para obtener una lista de valores decodificados, y la función `map2` para crear una nueva instancia del tipo `Usuario`. Luego, utilizamos `decodeString` para decodificar los datos del JSON.

## Ver también

- Documentación oficial de JSON en Elm: https://guide.elm-lang.org/effects/json.html
- Ejemplos de decodificación de JSON en Elm: https://github.com/brianthicks/elm-decode-examples