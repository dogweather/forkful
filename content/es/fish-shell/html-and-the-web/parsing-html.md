---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:01.235358-07:00
description: "El an\xE1lisis (parseo) de HTML se trata de extraer datos o informaci\xF3\
  n del contenido HTML, una tarea com\xFAn al trabajar con datos web. Los programadores\u2026"
lastmod: '2024-02-25T18:49:55.968774-07:00'
model: gpt-4-0125-preview
summary: "El an\xE1lisis (parseo) de HTML se trata de extraer datos o informaci\xF3\
  n del contenido HTML, una tarea com\xFAn al trabajar con datos web. Los programadores\u2026"
title: Analizando HTML
---

{{< edit_this_page >}}

## Qué y Por Qué?

El análisis (parseo) de HTML se trata de extraer datos o información del contenido HTML, una tarea común al trabajar con datos web. Los programadores hacen esto para automatizar la extracción de información de sitios web, para tareas como el web scraping, la minería de datos o las pruebas automatizadas.

## Cómo hacerlo:

Predominantemente, Fish shell no está diseñado para analizar HTML directamente. Sin embargo, sobresale en unir herramientas Unix como `curl`, `grep`, `sed`, `awk`, o utilizando herramientas especializadas como `pup` o `beautifulsoup` en un script de Python. A continuación, se presentan ejemplos que muestran cómo aprovechar estas herramientas desde Fish shell para analizar HTML.

### Usando `curl` y `grep`:
Obtener contenido HTML y extraer líneas que contienen enlaces:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Salida:
```
/page1.html
/page2.html
...
```

### Usando `pup` (una herramienta de línea de comandos para analizar HTML):

Primero, asegúrate de que `pup` esté instalado. Luego puedes usarlo para extraer elementos por sus etiquetas, ids, clases, etc.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

La salida, similar al ejemplo de `grep`, listaría los atributos href de las etiquetas `<a>`.

### Con un script de Python y `beautifulsoup`:

Aunque Fish por sí mismo no puede analizar HTML de forma nativa, se integra perfectamente con scripts de Python. A continuación, un ejemplo conciso que usa Python con `BeautifulSoup` para analizar y extraer títulos de HTML. Asegúrate de tener instalados `beautifulsoup4` y `requests` en tu entorno Python.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Uso:

```fish
parse_html 'https://example.com'
```

Salida:
```
Example Domain
```

Cada uno de estos métodos sirve para diferentes casos de uso y escalas de complejidad, desde la simple manipulación de texto de línea de comandos hasta el pleno poder de análisis de `beautifulsoup` en scripts de Python. Dependiendo de tus necesidades y la complejidad de la estructura HTML, puedes elegir un enfoque de línea de comandos sencillo o un enfoque de script más poderoso.
