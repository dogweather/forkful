---
date: 2024-01-20 17:34:49.613199-07:00
description: "C\xF3mo hacerlo: Hist\xF3ricamente, la concatenaci\xF3n de cadenas es\
  \ tan antigua como los primeros lenguajes de programaci\xF3n, la necesidad de manipular\
  \ texto ha\u2026"
lastmod: '2024-04-05T21:54:00.455923-06:00'
model: gpt-4-1106-preview
summary: "Hist\xF3ricamente, la concatenaci\xF3n de cadenas es tan antigua como los\
  \ primeros lenguajes de programaci\xF3n, la necesidad de manipular texto ha sido\
  \ siempre fundamental."
title: "Concatenaci\xF3n de cadenas de texto"
weight: 3
---

## Cómo hacerlo:
```Haskell
-- Concatenando con el operador ++
saludoCompleto = "Hola, " ++ "mundo!"
-- saludoCompleto es "Hola, mundo!"

-- Concatenando varias cadenas con ++
frase = "Haskell " ++ "es " ++ "genial."
-- frase es "Haskell es genial."

-- Usando concat y listas
nombres = concat ["Pedro", " y ", "Juana"]
-- nombres es "Pedro y Juana"

-- Usando la función unwords para unir palabras con espacios
palabrasUnidas = unwords ["Listas", "de", "palabras", "unidas."]
-- palabrasUnidas es "Listas de palabras unidas."
```

## Profundización:
Históricamente, la concatenación de cadenas es tan antigua como los primeros lenguajes de programación, la necesidad de manipular texto ha sido siempre fundamental. En Haskell, el operador `++` es la herramienta más directa para este fin, pero como Haskell es perezoso (lazy evaluation), concatenar listas largas puede ser ineficiente. Aquí entra `concat`, que maneja mejor listas de cadenas, o la función `unwords` que es ideal para unir palabras con espacios.

Hay alternativas más sofisticadas como el uso de `Data.Text` para trabajar con texto de manera más eficiente, especialmente en programas que realizan una gran cantidad de manipulaciones de texto.

En cuanto a implementación, Haskell maneja las cadenas como listas de caracteres, por lo que la concatenación implica recorrer la primera lista hasta el final antes de adjuntar la segunda, y así sucesivamente, lo cual es importante tener en cuenta en términos de desempeño.

## Ver Además:
- [Haskell Documentation for Lists (que incluye operaciones de concatenación)](https://haskell.org/documentation)
- [SO: Why is Haskell's ++ operator called the "append" operator?](https://stackoverflow.com/questions/30578839/why-is-haskells-operator-called-the-append-operator)
