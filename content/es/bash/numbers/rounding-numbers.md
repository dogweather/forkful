---
title:                "Redondeo de números"
aliases: - /es/bash/rounding-numbers.md
date:                  2024-01-26T03:42:37.095687-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Redondear números significa cortar los decimales a un valor más simple que es suficientemente bueno para un contexto dado. Los programadores redondean números para simplificar resultados, ahorrar espacio, o porque el valor exacto no es vital, como cuando estás estimando el uso de CPU o el espacio en disco, y los decimales no van a cambiar tu día.

## Cómo hacerlo:

Aquí tienes la esencia sobre el redondeo en Bash:

```Bash
# Redondear hacia abajo usando 'floor' con bc
echo "scale=0; 3.49/1" | bc

# Redondear hacia arriba usando 'ceiling' con bc
echo "scale=0; 3.01/1" | bc -l

# Redondear al entero más cercano usando printf
printf "%.0f\n" 3.49

# Un truco para redondear al entero más cercano usando bc
echo "(3.49+0.5)/1" | bc
```

Salidas de muestra—directamente de la boca del terminal:

```
3  # Redondeado hacia abajo (floor)
4  # Redondeado hacia arriba (ceiling)
3  # Redondeado al más cercano (con printf)
4  # Redondeado al más cercano (con bc)
```

## Análisis Detallado

En los viejos tiempos, no había `bc` ni `printf` en los scripts de Bash para hacer magia matemática. Los de la vieja escuela tenían que depender de herramientas externas o trucos ingeniosos. Ahora, `bc` te permite hacer matemáticas de precisión. Ten en cuenta, `bc` no redondea por defecto—realiza un floor. La parte de la escala establece la acción del punto decimal.

¿Alternativas? Podrías usar `awk` para redondear sin cambiar a `bc` o lidiar con `perl` para necesidades matemáticas más pesadas. Para los masoquistas, ve con Bash puro, digamos, manipulación de cadenas iterativas, pero ¿por qué?

En cuanto a detalles, `bc` no solo redondea, hace montones de cosas matemáticas—escalalo, sinelo, raíz cuadrado, lo que sea. Con `printf`, se trata más de formatear texto, pero oye, redondea números, así que no nos quejamos.

## Ver También

Para aquellos con hambre de más:

- Manual de GNU `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Comando `printf` de Bash: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- Guía del usuario de AWK (para redondeo y otro procesamiento de texto): https://www.gnu.org/software/gawk/manual/gawk.html
- Más matemáticas de Bash, scripting, y trucos numéricos: https://mywiki.wooledge.org/BashFAQ/022
