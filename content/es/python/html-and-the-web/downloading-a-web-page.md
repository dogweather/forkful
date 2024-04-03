---
date: 2024-01-20 17:44:31.361043-07:00
description: "Descargar una p\xE1gina web significa traer su contenido HTML a tu disco\
  \ local. Los programadores lo hacen para analizar la estructura de la p\xE1gina,\
  \ extraer\u2026"
lastmod: '2024-03-13T22:44:58.609544-06:00'
model: gpt-4-1106-preview
summary: "Descargar una p\xE1gina web significa traer su contenido HTML a tu disco\
  \ local."
title: "Descargando una p\xE1gina web"
weight: 42
---

## How To:
Usaremos `requests`, una biblioteca de Python fácil de usar para solicitudes HTTP:

```Python
import requests

# Enviar solicitud GET a una página web
respuesta = requests.get('https://example.com')

# Verificar que la solicitud fue exitosa
if respuesta.status_code == 200:
    # Escribir contenido a un archivo local
    with open('pagina_descargada.html', 'w', encoding='utf-8') as archivo:
        archivo.write(respuesta.text)

    print("Descarga completa!")
else:
    print("Error en la descarga:", respuesta.status_code)
```

Cuando ejecutes el código, verás "Descarga completa!" si todo va bien, o un mensaje de error con el código del estado HTTP si no.

## Deep Dive
En la historia, se usaba `urllib` para descargar páginas web, pero `requests` simplificó mucho las cosas. Si bien `requests` es perfecto para tareas simples, hay otras librerías como `Scrapy` para scraping avanzado o `Selenium` cuando necesitas imitar un navegador real interactuando con JavaScript. Internamente, `requests` maneja cosas como mantener la sesión o gestionar cookies, lo que nos libera de ese peso.

## See Also
- Documentación de `requests`: https://docs.python-requests.org/en/latest/
- Tutorial de `Scrapy`: https://docs.scrapy.org/en/latest/intro/tutorial.html
- Selenium con Python: https://selenium-python.readthedocs.io/
