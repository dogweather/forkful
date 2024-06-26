---
date: 2024-01-20 17:56:42.248523-07:00
description: "C\xF3mo: Python facilita la lectura de argumentos de la l\xEDnea de\
  \ comandos usando el m\xF3dulo `sys`. Aqu\xED hay un ejemplo simple."
lastmod: '2024-03-13T22:44:58.629236-06:00'
model: gpt-4-1106-preview
summary: "Python facilita la lectura de argumentos de la l\xEDnea de comandos usando\
  \ el m\xF3dulo `sys`."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo:
Python facilita la lectura de argumentos de la línea de comandos usando el módulo `sys`. Aquí hay un ejemplo simple:

```python
import sys

if __name__ == "__main__":
    if len(sys.argv) > 1:
        for index, arg in enumerate(sys.argv[1:], start=1):
            print(f"Argumento {index}: {arg}")
    else:
        print("No se proporcionaron argumentos.")
```

Si guardas este script como `lector_args.py` y lo ejecutas desde la terminal así:

```
python lector_args.py hola mundo
```

Obtendrás:

```
Argumento 1: hola
Argumento 2: mundo
```

## Deep Dive
En la historia de la informática, los argumentos de la línea de comandos son tan antiguos como los propios sistemas operativos de tipo UNIX. Python provee acceso a ellos a través del módulo `sys`, aunque también hay alternativas como el módulo `argparse` que es más potente y ofrece funcionalidades adicionales, como parseo de argumentos, ayuda automática y manejo de errores.

La lista `sys.argv` contiene los argumentos en el orden en que se introducen, siendo `sys.argv[0]` el nombre del script. El módulo `argparse`, por otro lado, te permite definir argumentos esperados y parsearlos de una manera más sofisticada, ayudándote a construir interfaces de línea de comandos más robustas con poco esfuerzo.

## Ver También
- [Documentación oficial de sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Tutorial de argparse](https://docs.python.org/3/howto/argparse.html)
- [PEP 389, la propuesta de argparse](https://www.python.org/dev/peps/pep-0389/)
- [Unix Programming FAQ - Command-line Arguments](http://www.faqs.org/faqs/unix-faq/programmer/faq/)
