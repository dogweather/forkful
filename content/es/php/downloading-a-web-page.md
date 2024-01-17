---
title:                "Descargando una página web"
html_title:           "PHP: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qué y Por qué?

La descarga de una página web se refiere a la acción de obtener el contenido de una página web y guardarlo en una computadora o dispositivo. Los programadores a menudo usan la descarga de páginas web en sus proyectos para obtener información de otras páginas o para facilitar la visualización de contenido en línea.

## Cómo hacerlo:

```PHP
// Utilizando la función file_get_contents()
$pagina = file_get_contents("https://ejemplo.com"); 
echo $pagina;

// Utilizando cURL
$curl = curl_init();
curl_setopt($curl, CURLOPT_URL, "https://ejemplo.com");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);
$pagina = curl_exec($curl);
echo $pagina;
```

## Profundizando

La descarga de páginas web se ha vuelto cada vez más importante con el crecimiento de la web y la abundancia de información en línea. Algunos programadores prefieren utilizar la función file_get_contents() debido a su simplicidad, mientras que otros prefieren utilizar cURL debido a su mayor versatilidad y opciones de configuración.

Es importante tener en cuenta que el uso de la descarga de páginas web también puede tener implicaciones legales si se utiliza para obtener información de forma ilegal o sin permiso del propietario del sitio web.

## Ver También

- [Documentación de file_get_contents()](https://www.php.net/manual/es/function.file-get-contents.php)
- [Documentación de cURL](https://www.php.net/manual/es/book.curl.php)
- [Tutorial de descarga de páginas web en PHP](https://www.php.net/manual/es/function.file-get-contents.php#example-4822)