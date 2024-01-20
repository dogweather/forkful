---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El análisis de una fecha a partir de una cadena es el proceso de recuperación de una fecha a partir de un texto. Los programadores lo hacen para facilitar el procesamiento de la fecha por parte de los sistemas informáticos.

## Como hacerlo:

Bash nos proporciona una herramienta llamada `date` para analizar la fecha. Aquí se muestra cómo usarla.

```Bash
fecha="31-12-2021"
fecha_formateada=$(date -d"$fecha" +"%Y-%m-%d")

echo $fecha_formateada
```

El output será:
```Bash
2021-12-31
```

Aquí, `-d` permite a `date` interpretar la fecha en diferentes formatos y `+"%Y-%m-%d"` formatea la fecha en el formato que especificamos.

## Profundizando

En el pasado, los programadores tenían que hacer mucho trabajo manual para analizar las fechas. Bash simplifica este proceso con la herramienta `date`.

Existen alternativas a `date` en Bash. Por ejemplo, puedes usar `awk` o `sed` para manipular cadenas y analizar la fecha.

Mayormente la implementación se basa en las bibliotecas subyacentes del sistema operativo, en el caso de Bash en Unix, principalmente glibc o cualquier otra biblioteca de C que el sistema este utilizando.

## Ver También

- [Comandos de la fecha de Bash en Linux Geeks](https://linuxhint.com/bash_date_command/)
- [Análisis de fecha de cadena en Stackoverflow](https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash)
- [Manipulando cadenas en Bash](https://tldp.org/LDP/abs/html/string-manipulation.html)