---
title:                "Fish Shell: Usando expresiones regulares"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué
Muchos programadores utilizan expresiones regulares para buscar y manipular patrones en texto. Esto puede ser útil para tareas como validar entradas de usuario, buscar y reemplazar texto en un archivo o filtrar datos de texto.

## Cómo
Utilizar expresiones regulares en Fish Shell es fácil y poderoso. A continuación, se presentan algunos ejemplos básicos:

```Fish Shell
# Buscar una palabra específica en un texto
grep "patrón" archivo.txt

# Reemplazar una palabra por otra en un archivo
sed "s/palabra1/palabra2/" archivo.txt

# Filtrar líneas que contengan un patrón específico
awk '/patrón/ {print}' archivo.txt
```

## Profundizando
Las expresiones regulares permiten buscar patrones más complejos utilizando caracteres especiales que tienen un significado específico. Algunos ejemplos son:

- `.`: Cualquier carácter
- `*`: Cualquier cantidad de veces (incluyendo cero)
- `+`: Una o más veces
- `[]`: Cualquier carácter dentro de los corchetes
- `^`: Comienzo de una línea
- `$`: Fin de una línea

Estos son solo algunos de los caracteres especiales disponibles. Además, Fish Shell tiene una función incorporada llamada `string match` que también acepta expresiones regulares.

## Ver también
- [Documentación de expresiones regulares en Fish Shell](https://fishshell.com/docs/current/cmds/regexp.html)
- [Tutorial de expresiones regulares en español](https://www.ionos.com/digitalguide/websites/desarrollo-web/que-son-las-expresiones-regulares/)
- [Libro gratuito sobre regex en español](https://uanestevez.github.io/casos-reales-regexp/main.pdf)