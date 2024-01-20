---
title:                "Capitalizando una cadena de texto"
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programadores lo hacen para formatear textos, como títulos o nombres propios, respetando normas de escritura.

## Cómo hacerlo:

Para capitalizar una cadena en Fish, puedes usar la función `string` con la opción `capitalize`. Aquí tienes un ejemplo:

```Fish Shell
set frase "hola mundo desde fish"
echo $frase | string capitalize
```

Salida:

```
Hola Mundo Desde Fish
```

## En Profundidad:

Antes, en los scripts de shell, capitalizar una cadena era un rollo. Tenías que apañártelas con `awk`, `sed` o `tr` para conseguirlo. Pero Fish hace que sea pan comido con su función `string`, que ya trae soporte incorporado para este tipo de operaciones.

Alternativamente, podrías usar `awk` como en otros shells:

```Fish Shell
echo "hola mundo desde awk" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) tolower(substr($i,2))}1'
```

La implementación en Fish es moderna y directa, evitando los pequeños líos que suponían las alternativas antiguas, especialmente en scripts complejos.

## Ver También:

- Documentación oficial de Fish sobre el comando `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutorial de Fish Shell para principiantes: https://fishshell.com/docs/current/tutorial.html
- Foro de la comunidad Fish: https://fishshell.com/community.html

Esto te dará más contexto e ideas sobre cómo manipular cadenas y otros datos en Fish.