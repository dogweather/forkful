---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

#Descargando una Página Web con PowerShell

## ¿Qué y Por qué?

"Descargar una página web” significa recuperar el código HTML de la página. Los programadores lo hacen por muchas razones, incluida la recopilación de datos para análisis o automatizando interacciones con la web.

## ¿Cómo hacerlo?

Para descargar una página web con PowerShell, puedes usar el cmdlet `Invoke-WebRequest`. Aquí tienes un ejemplo de cómo hacerlo:

```PowerShell
$url= "http://tuweb.com"
$paginaWeb = Invoke-WebRequest -Uri $url
echo $paginaWeb.Content
```

Esto imprimirá en la consola el contenido HTML de la página. 

## Plongee

## Un vistazo al pasado
PowerShell se introdujo en 2006 e incorporó el cmdlet `Invoke-WebRequest` en la versión 3.0 que salió en 2012. Desde entonces, se ha convertido en una herramienta esencial para los programadores que trabajan con la web.

## Alternativas
Existen muchas alternativas para descargar una página web, incluyendo librerías y módulos en otros lenguajes de programación, como `urllib` en Python o` http` en Node.js.

## Detalles de implementación
Cuando ejecutas `Invoke-WebRequest` en PowerShell, el sistema enviará una solicitud HTTP GET al servidor del URL proporcionado. Luego el servidor responderá con el documento HTML de la página web, que será accesible a través de la propiedad `Content` que mostramos en el ejemplo.

## Ver También

- [Documentación oficial de `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [Artículo adicional sobre descarga de archivos con PowerShell](http://www.powershellpro.com/powershell-tutorial-introduction/utilities-web-requests/)
- [Tutorial en video sobre scrapping con PowerShell](https://www.youtube.com/watch?v=q2e97d2Y0gY)

Nota que PowerShell es una herramienta poderosa que facilita las tareas de programación. Recuerda siempre usarla respentado las políticas de privacidad y los términos de uso de los sitios web que accedes. ¡Diviértete programando!