---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares son patrones usados para encontrar coincidencias en cadenas de texto. Los programadores las utilizan para buscar, validar, o manipular datos de manera eficiente y con pocas líneas de código.

## Cómo Hacerlo:
El uso de expresiones regulares en Fish se realiza principalmente a través de comandos externos como `grep`, `sed`, o `awk`, ya que Fish no tiene un soporte integrado para ellos. Aquí algunos ejemplos:

```Fish Shell
# Buscar coincidencia de "foo" en un archivo
grep "foo" archivo.txt

# Reemplazar todas las instancias de "foo" por "bar" en un archivo
sed "s/foo/bar/g" archivo.txt

# Extraer líneas que contengan dígitos
grep "[0-9]" archivo.txt 
```

Resultado de `grep "foo" archivo.txt` si `foo` está en el texto:
```
foo está aquí
```

## Profundización
Las expresiones regulares se originaron en la década de 1950, con notables contribuciones posteriores de Ken Thompson en el contexto de herramientas de línea de comandos de Unix. Aunque el shell Fish no viene con soporte nativo para regex, se integra sin problemas con herramientas de terceros que son estándar en sistemas Unix-like. Alternativas modernas para trabajar con texto incluyen lenguajes de programación como Python, con su potente módulo `re`, o herramientas como `ripgrep` que son más rápidas que `grep`.

## Ver También
- GNU Grep: https://www.gnu.org/software/grep/manual/grep.html
- Sed - an Introduction and Tutorial: https://www.grymoire.com/Unix/Sed.html
- AWK: A Tutorial and Introduction: https://www.grymoire.com/Unix/Awk.html
- Regular-Expressions.info: https://www.regular-expressions.info/
- ripgrep: https://github.com/BurntSushi/ripgrep
