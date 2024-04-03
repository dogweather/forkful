---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:31.447971-07:00
description: "Las expresiones regulares (regex) en PHP son patrones utilizados para\
  \ coincidir con combinaciones de caracteres en cadenas de texto, permitiendo\u2026"
lastmod: '2024-03-13T22:44:59.150749-06:00'
model: gpt-4-0125-preview
summary: "Las expresiones regulares (regex) en PHP son patrones utilizados para coincidir\
  \ con combinaciones de caracteres en cadenas de texto, permitiendo operaciones de\
  \ b\xFAsqueda y reemplazo sofisticadas y validaci\xF3n de datos."
title: Usando expresiones regulares
weight: 11
---

## ¿Qué y Por Qué?

Las expresiones regulares (regex) en PHP son patrones utilizados para coincidir con combinaciones de caracteres en cadenas de texto, permitiendo operaciones de búsqueda y reemplazo sofisticadas y validación de datos. Los programadores aprovechan regex por su potencia y flexibilidad en el análisis de texto, validación de formularios o scraping de datos web, convirtiéndolo en una herramienta indispensable en el arsenal de un desarrollador.

## Cómo utilizarlas:

PHP soporta las expresiones regulares a través de la biblioteca PCRE (Perl Compatible Regular Expressions), ofreciendo un rico conjunto de funciones. Así es cómo se utilizan:

### Coincidir con un patrón:

Para verificar si un patrón existe dentro de una cadena, se utiliza `preg_match()`. Esta función devuelve 1 si el patrón fue encontrado en la cadena y 0 si no.

```php
if (preg_match("/\bweb\b/i", "PHP es un lenguaje de scripting web")) {
    echo "Se encontró una coincidencia.";
} else {
    echo "No se encontró una coincidencia.";
}
// Salida: Se encontró una coincidencia.
```

### Encontrando todas las coincidencias:

`preg_match_all()` se utiliza cuando necesitas encontrar todas las ocurrencias de un patrón dentro de una cadena.

```php
$texto = "gatos y perros";
$patrón = "/\b([a-z]+)\b/i";
preg_match_all($patrón, $texto, $coincidencias);
print_r($coincidencias[0]);
// Salida: Array ( [0] => gatos [1] => y [2] => perros )
```

### Reemplazando texto:

Para reemplazar texto que coincide con una expresión regular, se utiliza `preg_replace()`. Es increíblemente poderoso para formatear y limpiar datos.

```php
$textoOriginal = "15 de abril de 2003";
$patrón = "/(\w+) (\d+), (\d+)/i";
$reemplazo = '${1}1,$3';
echo preg_replace($patrón, $reemplazo, $textoOriginal);
// Salida: 15 de abril1,2003
```

### Dividiendo cadenas:

Puedes dividir una cadena en un arreglo usando `preg_split()`, especificando un patrón para el delimitador.

```php
$texto = "PHP es, un lenguaje de scripting, extremadamente popular";
$partes = preg_split("/,\s*/", $texto);
print_r($partes);
// Salida: Array ( [0] => PHP es [1] => un lenguaje de scripting [2] => extremadamente popular )
```

Además, para patrones regex complejos y tareas, marcos y bibliotecas como el componente `Finder` de Symfony o la colección de funciones de ayuda de Laravel podrían proporcionar una capa de abstracción más conveniente. Sin embargo, entender y utilizar las funciones PCRE integradas en PHP es crucial para el procesamiento de texto eficiente y la validación directamente dentro de los scripts de PHP.
