---
title:                "Analizando HTML"
html_title:           "PowerShell: Analizando HTML"
simple_title:         "Analizando HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El análisis de HTML es una técnica utilizada por los programadores para extraer información específica de una página web. Esto les permite automatizar ciertas tareas y acceder a datos de forma más eficiente.

## Cómo hacerlo:

El siguiente código en PowerShell ilustra cómo analizar un archivo HTML utilizando el módulo "Invoke-WebRequest" de PowerShell. Este código extraerá todos los títulos de las noticias de la página de inicio de un sitio web y los imprimirá en la consola.

```PowerShell
# Descargar el contenido HTML de la página de inicio
$html = Invoke-WebRequest -Uri "https://www.ejemplo.com/"

# Encontrar todos los elementos <h1> y almacenarlos en una variable
$titulos = $html.ParsedHtml.getElementsByTagName("h1")

# Iterar sobre los elementos y mostrar su texto
foreach ($titulo in $titulos) {
    Write-Host $titulo.innerText
}
```

La salida de este código sería algo como:

```
Bienvenidos a ejemplo.com
Últimas noticias
Cambia tu vida con nuestro nuevo curso en línea
Nuevo lanzamiento de producto: ¡No te lo pierdas!
```

## Profundizando:

El análisis de HTML ha sido una técnica utilizada desde los primeros días de la web para extraer datos de páginas web. Sin embargo, también existen otras formas de extraer datos, como la API de un sitio o el uso de herramientas de scraping dedicadas.

El módulo "Invoke-WebRequest" de PowerShell utiliza la clase "HtmlDocument" del lenguaje .NET para analizar el HTML. Esto significa que también se pueden utilizar otros lenguajes de programación que utilicen .NET para lograr el mismo resultado.

## Ver también:

- Documentación oficial de "Invoke-WebRequest": https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7