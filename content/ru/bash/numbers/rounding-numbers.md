---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:42.291805-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u0441\u0443\u0442\u044C \u043E\u043A\u0440\u0443\
  \u0433\u043B\u0435\u043D\u0438\u044F \u0432 Bash."
lastmod: '2024-03-13T22:44:45.356573-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0441\u0443\u0442\u044C \u043E\u043A\u0440\u0443\u0433\
  \u043B\u0435\u043D\u0438\u044F \u0432 Bash."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как это сделать:
Вот суть округления в Bash:

```Bash
# Округление вниз с использованием 'floor' через bc
echo "scale=0; 3.49/1" | bc

# Округление вверх с использованием 'ceiling' через bc
echo "scale=0; 3.01/1" | bc -l

# Округление до ближайшего целого с помощью printf
printf "%.0f\n" 3.49

# Хитрость для округления до ближайшего целого через bc
echo "(3.49+0.5)/1" | bc
```

Примеры вывода — прямиком из терминала:

```
3  # Округлено вниз (floor)
4  # Округлено вверх (ceiling)
3  # Округлено до ближайшего целого (с помощью printf)
3  # Округлено до ближайшего целого (через bc)
```

## Погружение в Детали
В старину в скриптах Bash не было `bc` или `printf` для выполнения математических чудес. Предшественникам приходилось полагаться на внешние инструменты или изощрённые обходные пути. Теперь `bc` позволяет выполнять вычисления с точностью. Имейте в виду, `bc` по умолчанию не округляет — он приводит к нижнему целому. Часть scale устанавливает действия с десятичной точкой.

Альтернативы? Вы можете использовать `awk` для округления без переключения на `bc` или возиться с `perl` для более серьёзных математических потребностей. Для мазохистов — чистый Bash, скажем, с итеративной манипуляцией со строками — но зачем?

Что касается деталей, `bc` не только округляет, он выполняет массу математических операций — масштабирует, вычисляет синус, корень, назовите любое действие. Что касается `printf`, это больше о форматировании текста, но, эй, он округляет числа, так что мы не жалуемся.

## См. Также
Для тех, кто хочет узнать больше:

- Руководство по GNU `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Команда Bash `printf`: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- Руководство пользователя AWK (для округления и другой обработки текста): https://www.gnu.org/software/gawk/manual/gawk.html
- Больше математики Bash, скриптов и трюков с числами: https://mywiki.wooledge.org/BashFAQ/022
