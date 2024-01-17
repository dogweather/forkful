---
title:                "Trabajando con yaml"
html_title:           "Elixir: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con YAML es una forma común para que los programadores manejen y guarden datos estructurados. Se utiliza para archivos de configuración, intercambio de datos y más debido a su sintaxis simple y legible.

## ¿Cómo hacerlo?
El lenguaje Elixir proporciona una librería llamada YAML que nos permite interactuar con archivos y datos YAML fácilmente. A continuación, se muestran algunos ejemplos de cómo usarla:

### Cargar un archivo YAML
```
Elixir
yaml = YAML.load_file("config.yml")
```

### Obtener un valor específico
```
Elixir
yaml["config"]["url"]
# => "https://www.example.com"
```

### Guardar datos en un archivo YAML
```
Elixir
data = %{username: "johndoe", password: "12345"}
YAML.store("data.yml", data)
```

### Convertir un mapa a formato YAML
```
Elixir
data = %{name: "Elixir", version: "1.10.3"}
YAML.dump(data)
# => "---\n:name: \"Elixir\"\n:version: \"1.10.3\""
```

## Inmersión profunda
YAML, que significa "YAML Ain't Markup Language", es un formato de serialización de datos que fue creado en el año 2001. Además de la librería YAML de Elixir, hay otras opciones como YamlElixir o Yamlix que también pueden ser utilizadas en proyectos.

## Ver también
- Sitio oficial de la librería YAML para Elixir: https://hexdocs.pm/yaml/
- Otros paquetes de Elixir relacionados con YAML: YamlElixir (https://github.com/jeremyong/yaxpeax-elixir) y Yamlix (https://github.com/angelikatyborska/yaml-eex)