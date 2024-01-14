---
title:                "C#: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML

Si eres un programador en C# y buscas una manera más sencilla de manejar archivos de configuración, YAML es una excelente opción. YAML es un formato de datos legible por humanos que permite estructurar información de manera más clara y concisa.

## Cómo utilizar YAML en C#

Para trabajar con YAML en C#, necesitarás un paquete de NuGet llamado "YamlDotNet". Puedes instalarlo desde la consola de NuGet usando el comando ``Install-Package YamlDotNet``, o puedes agregarlo manualmente a tu proyecto.

Una vez que tengas el paquete instalado, puedes comenzar a utilizar YAML en tu código. Aquí hay un ejemplo básico de cómo leer y escribir un archivo YAML:

```C#
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

// Crear un objeto Serializer con la convención de nomenclatura de C#
var serializer = new SerializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .Build();

// Leer un archivo YAML en un objeto
var fileContents = File.ReadAllText("config.yaml");
var config = serializer.Deserialize<Config>(fileContents);

// Escribir un objeto a un archivo YAML
var yaml = serializer.Serialize(config);
File.WriteAllText("config2.yaml", yaml);
```

La clase `Config` debe tener propiedades públicas que coincidan con las claves en tu archivo YAML. Por ejemplo:

```C#
public class Config
{
    public string Nombre { get; set; }
    public int Edad { get; set; }
}
```

Esto corresponderá a un archivo YAML como el siguiente:

```yaml
nombre: Juan
edad: 25
```

## Profundizando en YAML

Además de estructurar archivos de configuración, YAML también puede ser utilizado para transferir datos entre diferentes sistemas. Una de las características más útiles de YAML para los desarrolladores de C# es la capacidad de serializar y deserializar objetos complejos.

Puedes realizar una serialización personalizada al crear tus propios convertidores. Esto te permite controlar cómo se convierten tus objetos en YAML y viceversa. Puedes encontrar más información sobre esto en la documentación de YamlDotNet.

También es importante tener en cuenta que YAML es sensible a la indentación y utiliza dos puntos para separar claves y valores. Por lo tanto, asegúrate de seguir estas reglas cuando creas o editas archivos YAML.

## Ver también

- Documentación de YamlDotNet: https://github.com/aaubry/YamlDotNet
- Guía de sintaxis YAML: https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html
- Ejemplos de uso de YAML en aplicaciones C#: https://www.codeproject.com/Articles/1166887/Using-YamlDotNet-for-Configuring-Your-NET-Applicati