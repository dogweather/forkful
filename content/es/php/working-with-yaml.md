---
title:                "PHP: Trabajando con yaml."
simple_title:         "Trabajando con yaml."
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por qué
El lenguaje de programación PHP es una herramienta muy versátil y ampliamente utilizado en la programación web. Una de las razones por las que es tan popular es debido a la flexibilidad que ofrece a los desarrolladores para trabajar con diferentes formatos de datos, incluyendo YAML. 

YAML (YAML Ain't Markup Language) es un formato de datos legible para humanos y fácil de usar, ideal para almacenar y transferir datos estructurados. Al trabajar con PHP y YAML, se puede lograr una mayor eficiencia en la gestión de datos y una mejor organización del código.

##Cómo hacerlo
Para trabajar con YAML en PHP, es necesario utilizar una biblioteca externa llamada "Symfony YAML". Esta biblioteca permite leer y escribir en archivos YAML y convertirlo a un formato compatible con PHP.

A continuación, se muestra un ejemplo de cómo leer un archivo YAML usando la biblioteca Symfony:

```PHP
$yaml = file_get_contents('datos.yml');
$datos = Yaml::parse($yaml);

print_r($datos);
```

En el código anterior, primero se abre y lee el archivo YAML y luego se utiliza la función `parse()` para convertirlo en un array de PHP que se puede imprimir con `print_r()` para visualizar su contenido.

##Profundizando
Además de leer y escribir archivos YAML, la biblioteca Symfony también permite la validación y el mapeo de datos. También ofrece opciones para personalizar la forma en que se maneja la indentación y otros detalles de formato.

Para aquellos que deseen profundizar aún más en el trabajo con YAML en PHP, se pueden consultar recursos como la documentación oficial de Symfony YAML y otros tutoriales en línea.

## Ver también
- [Documentación oficial de Symfony YAML](https://symfony.com/doc/current/components/yaml.html)
- [Tutorial de TutsPlus sobre YAML y PHP](https://code.tutsplus.com/es/tutorials/working-with-yaml---cms-31582)
- [Guía de inicio rápido de YAML y PHP](https://dev.to/fruitysheep/working-with-yaml-on-php--suit)