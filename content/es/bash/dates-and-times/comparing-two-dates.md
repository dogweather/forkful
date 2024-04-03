---
date: 2024-01-20 17:32:15.292828-07:00
description: "C\xF3mo: ."
lastmod: '2024-03-13T22:44:59.259395-06:00'
model: gpt-4-1106-preview
summary: .
title: "Comparaci\xF3n de dos fechas"
weight: 27
---

## Cómo:
```Bash
# Consigue la fecha actual en formato Año-Mes-Día
hoy=$(date +%F)

# Ejemplo de fecha para comparar, p.e. la fecha de entrega
fecha_entrega="2023-05-15"

# Compara las fechas
if [[ "$hoy" > "$fecha_entrega" ]]; then
  echo "La fecha de entrega ya pasó."
elif [[ "$hoy" < "$fecha_entrega" ]]; then
  echo "Aún hay tiempo antes de la fecha de entrega."
else
  echo "Hoy es la fecha de entrega."
fi
```
Salida posible:
```
Aún hay tiempo antes de la fecha de entrega.
```

## Deep Dive
Bash usa GNU `date` para manipular y comparar fechas. Antes de Bash versión 4, comparar fechas era un dolor: tenías que convertir fechas a segundos desde la "época" (1970-01-01) y luego comparar los números. Ahora, con el operador `[[`, lo hacemos directo. Hay alternativas: usar `date` para manipular fechas o `awk`. Pero estas pueden ser más complicadas o lentas.

## Ver También
- `man date`: para más detalles sobre el comando `date`.
- [Comparación de cadenas en Bash](https://tldp.org/LDP/abs/html/string-manipulation.html): sobre la comparación de cadenas de texto, como la que usamos con fechas.
- [Stack Overflow: Comparar fechas en Bash](https://stackoverflow.com/questions/22636996/how-to-compare-dates-in-bash): discusiones y ejemplos adicionales de comparación de fechas.
