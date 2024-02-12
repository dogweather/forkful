---
title:                "Trabajando con YAML"
aliases: - /es/elm/working-with-yaml.md
date:                  2024-02-03T19:25:19.013433-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Elm no tiene soporte incorporado para YAML, un formato de serialización de datos a menudo utilizado para archivos de configuración o compartir datos, debido a su fuerte énfasis en la seguridad de tipos y resultados predecibles. Sin embargo, los programadores frecuentemente encuentran YAML al tratar con APIs o configuraciones en el desarrollo web, lo que requiere métodos confiables para analizar datos en YAML en el ecosistema estrictamente tipado de Elm para su integración y manipulación sin problemas.

## Cómo hacerlo:

Para manejar YAML en Elm, generalmente necesitas convertir YAML a JSON fuera de Elm y luego usar la funcionalidad incorporada de decodificador JSON de Elm para trabajar con los datos. Aunque este enfoque requiere un paso adicional de conversión, aprovecha el fuerte sistema de tipos de Elm para asegurar la integridad de los datos. Herramientas populares para la conversión de YAML a JSON incluyen convertidores en línea o servicios de backend. Una vez que tienes JSON, puedes usar el módulo `Json.Decode` de Elm para trabajar con los datos.

Primero, asumiendo que tienes los siguientes datos en YAML:

```yaml
persona:
  nombre: Jane Doe
  edad: 30
```

Conviértelos al formato JSON:

```json
{
  "persona": {
    "nombre": "Jane Doe",
    "edad": 30
  }
}
```

Luego, define tu modelo y decodificador en Elm:

```elm
módulo Principal expone (..)

importar Html exponiendo (texto)
importar Json.Decode como Decode

type alias Persona =
    { nombre : String
    , edad : Int
    }

decodificadorPersona : Decode.Decoder Persona
decodificadorPersona =
    Decode.map2 Persona
        (Decode.field "nombre" Decode.string)
        (Decode.field "edad" Decode.int)
        
```

Para usar este decodificador para convertir JSON a un tipo Elm:

```elm
importar Json.Decode como Decode

cadenaJson = 
    """
    {
      "persona": {
        "nombre": "Jane Doe",
        "edad": 30
      }
    }
    """

resultadoDecodificacion = Decode.decodeString (Decode.field "persona" decodificadorPersona) cadenaJson

principal =
    case resultadoDecodificacion of
        Ok persona ->
            Html.text ("Hola, " ++ persona.nombre ++ "!")
            
        Err _ ->
            Html.text "Ocurrió un error al decodificar."
```

Salida (renderizada en una aplicación Elm):
```
Hola, Jane Doe!
```

Este enfoque asegura que puedes trabajar con datos en YAML en Elm utilizando JSON como formato intermedio, aprovechando el robusto sistema de tipos de Elm y las capacidades de decodificación JSON para manipular datos externos de manera segura y efectiva.
