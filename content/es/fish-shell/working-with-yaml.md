---
title:                "Trabajando con yaml"
html_title:           "Fish Shell: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador o un sysadmin, es muy probable que hayas escuchado sobre YAML o tal vez incluso hayas trabajado con él. YAML es un lenguaje de serialización de datos que se utiliza para estructurar y almacenar información de manera legible para humanos. Es muy útil en el mundo de la programación ya que nos permite almacenar datos de manera organizada y fácilmente accesible.

## Cómo usar YAML en Fish Shell

Para empezar a trabajar con YAML en Fish Shell, todo lo que necesitas es instalar el paquete 'yq'. Puedes hacerlo usando el gestor de paquetes de Fish Shell:

```Fish Shell

fisher install yq

```

Una vez instalado, puedes comenzar a utilizar los comandos de yq para leer, escribir o modificar archivos YAML. Por ejemplo, si tienes un archivo YAML llamado 'config.yml' con la siguiente estructura:

```YAML

user:
    name: John
    email: john@example.com
```

Puedes utilizar el siguiente comando para leer el valor de 'email':

```Fish Shell

yq read config.yml user.email

```

Y obtendrás como resultado:

```Shell

john@example.com

```

También puedes utilizar yq para modificar valores en un archivo YAML. Por ejemplo, si quieres cambiar el correo electrónico de John a 'john.new@example.com', puedes utilizar el siguiente comando:

```Fish Shell

yq write config.yml user.email john.new@example.com

```

Si verificas el archivo 'config.yml', verás que el valor de 'email' ha sido actualizado.

## Deep Dive

Además de leer y escribir archivos YAML, yq también ofrece una amplia variedad de comandos para trabajar con este formato de datos. Puedes utilizar yq para filtrar datos, crear nuevos documentos YAML, combinar varios archivos YAML y mucho más.

También es importante mencionar que yq es compatible con la especificación YAML 1.2, lo que significa que puede manejar archivos YAML complejos con facilidad.

En caso de que necesites más información sobre cómo utilizar yq, puedes consultar su documentación oficial en GitHub. Allí encontrarás una descripción detallada de cada comando y ejemplos de uso.

## Ver también

- Documentación oficial de yq en GitHub: https://github.com/mklement0/yq/
- Tutorial sobre cómo trabajar con archivos YAML en Fish Shell: https://medium.com/@mauro_lineapeluffo/working-with-yaml-files-in-fish-shell-2744ff8559a5
- Guía completa de YAML para principiantes: https://blog.risingstack.com/yaml-tutorial-everything-you-need-to-start-with/