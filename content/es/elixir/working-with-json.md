---
title:                "Elixir: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Elixir?

JSON (JavaScript Object Notation) es un formato de intercambio de datos ampliamente utilizado en aplicaciones web y móviles. Elixir, siendo un lenguaje de programación funcional y dinámico, ofrece herramientas poderosas para manejar y manipular datos en formato JSON. Al dominar el trabajo con JSON en Elixir, podrás crear aplicaciones más robustas y escalables.

## Cómo trabajar con JSON en Elixir

La librería estándar de Elixir incluye un módulo llamado `Jason` que nos permite parsear y generar JSON de forma sencilla. Veamos un ejemplo de cómo podemos convertir un mapa de Elixir a JSON:

```Elixir
map = %{name: "Maria", age: 25, hobby: "painting"}
json = Jason.encode!(map)

IO.puts(json)
#=> {"name":"Maria","age":25,"hobby":"painting"}
```

También podemos hacer el proceso inverso, es decir, convertir un JSON a un mapa de Elixir:

```Elixir
json = "{\"city\":\"Madrid\",\"country\":\"Spain\",\"population\": 6.6}"
map = Jason.decode!(json)

IO.inspect(map)
#=> %{city: "Madrid", country: "Spain", population: 6.6}
```

Además de trabajar con JSON básico, el módulo `Jason` también nos permite trabajar con datos estructurados más complejos, como listas y anidaciones de objetos.

## Una mirada más profunda al trabajo con JSON en Elixir

Además del módulo `Jason`, también existen otras librerías de terceros como `Poison` y `JSEX` que ofrecen funcionalidades adicionales y diferencias en el rendimiento. Además, es importante conocer los estándares y convenciones de JSON, ya que esto te ayudará a escribir un código más legible y mantenible.

En Elixir, también podemos utilizar patrones de coincidencia de patrones para manejar errores al parsear o generar JSON, lo que hace que nuestro código sea más robusto y resistente a fallos.

## Consulta también

- [Documentación de la librería Jason en Elixir](https://github.com/michalmuskala/jason)
- [Diferencias entre las librerías Jason, Poison y JSEX](https://digitalfio.com/2016/08/22/json-parsing-in-elixir/)
- [Convenciones y buenas prácticas al trabajar con JSON en Elixir](https://hexdocs.pm/jason/playing_with_json.html#validating-json-schema)