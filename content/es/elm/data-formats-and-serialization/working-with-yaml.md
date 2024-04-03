---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:19.013433-07:00
description: "Elm no tiene soporte incorporado para YAML, un formato de serializaci\xF3\
  n de datos a menudo utilizado para archivos de configuraci\xF3n o compartir datos,\u2026"
lastmod: '2024-03-13T22:44:59.013873-06:00'
model: gpt-4-0125-preview
summary: "Elm no tiene soporte incorporado para YAML, un formato de serializaci\xF3\
  n de datos a menudo utilizado para archivos de configuraci\xF3n o compartir datos,\
  \ debido a su fuerte \xE9nfasis en la seguridad de tipos y resultados predecibles."
title: Trabajando con YAML
weight: 41
---

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
