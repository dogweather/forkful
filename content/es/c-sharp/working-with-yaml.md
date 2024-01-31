---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

YAML significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado). Sirve para serializar datos de manera legible por humanos, común en configuraciones y transmisión de datos. Programadores lo usan por su simplicidad y legibilidad, facilitando compartir y entender estructuras de datos sin mucho esfuerzo.

## Cómo hacerlo:

Para trabajar con YAML en C#, necesitarás una biblioteca como YamlDotNet. Aquí va cómo:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var alumno = new Alumno
        {
            Nombre = "Juan",
            Edad = 23,
            Curso = "C# Avanzado"
        };

        var serializador = new SerializerBuilder().WithNamingConvention(CamelCaseNamingConvention.Instance).Build();
        string yaml = serializador.Serialize(alumno);
        Console.WriteLine(yaml);
    }
}

public class Alumno
{
    public string Nombre { get; set; }
    public int Edad { get; set; }
    public string Curso { get; set; }
}
```

Salida esperada:

```yaml
nombre: Juan
edad: 23
curso: C# Avanzado
```

## La información detallada:

YAML nació en 2001 como un superconjunto de JSON, más humano-amigable. Es ampliamente usado en proyectos como Docker y Kubernetes. Alternativas incluyen JSON y XML, pero YAML sobresale cuando se necesita que el contenido sea accesible para personas. Detalles de implementación en C#: YamlDotNet utiliza `Serializer` para convertir objetos a YAML y `Deserializer` para pasar de YAML a objetos. Utiliza convenciones para mapear propiedades del objeto a claves YAML.

## Ver también:

- Documentación de YamlDotNet: https://github.com/aaubry/YamlDotNet/wiki
- Especificación de YAML: https://yaml.org/spec/1.2/spec.html
- Tutorial de YAML: https://www.tutorialspoint.com/yaml/index.htm
