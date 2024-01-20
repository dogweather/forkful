---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La concatenación de cadenas consiste en unir dos o más cadenas de texto en una sola. Los programadores la utilizan para combinar información, crear mensajes personalizados y manipular datos con mayor eficacia.

## Cómo Hacerlo:

```Bash
# Definiendo las cadenas

cadena1="Hola"
cadena2=", mundo!"

# Concatenando las cadenas

resultante="$cadena1$cadena2"
echo $resultante
```

Salida del código:

```Bash
Hola, mundo!
```

## Profundizando:

1. **Contexto histórico**: La concatenación de cadenas ha sido la base de la programación desde los primeros lenguajes como Fortran y COBOL. Bash hereda su funcionalidad de concatenación de cadenas directamente de estos lenguajes y otros shelle de Unix.

2. **Alternativas**: Si bien la concatenación directa es la forma más simple y eficiente de hacerlo, también puedes usar el comando `printf` para una concatenación más formato.

    ```Bash
    printf -v resultante "%s%s" "$cadena1" "$cadena2"
    echo $resultante
    ```

3. **Detalles de implementación**: En Bash, las cadenas se concatenan simplemente colocándolas una junto a la otra. Sin embargo, debe tener cuidado con los espacios, ya que estos son delimitadores en bash y no se incluyen automáticamente en la concatenación.

## Consulte Además:

- Guía de Bash: https://www.gnu.org/software/bash/manual/bash.html
- Concatenación de cadenas en otros lenguajes: https://en.wikipedia.org/wiki/Concatenation
- Alternativas a la concatenación de cadenas: https://stackoverflow.com/questions/4181703/bash-string-concatenation-how-to