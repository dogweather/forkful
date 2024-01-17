---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "PHP: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y Por qué?
Enviar un solicitud HTTP con autenticación básica es un proceso que permite a los programadores acceder a recursos protegidos en una aplicación web. Esto es necesario cuando se necesita autenticar el usuario antes de permitir el acceso a ciertas funciones o información. Los programadores utilizan este método para garantizar la seguridad de los datos y protegerlos de accesos no autorizados.

## Cómo hacerlo:
```PHP
$codigo = base64_encode("usuario:contraseña");
$encabezados = array(
    'Authorization: Basic '.$codigo
);

$solicitud = curl_init("https://ejemplo.com");
curl_setopt($solicitud, CURLOPT_HTTPHEADER, $encabezados);
$respuesta = curl_exec($solicitud);
curl_close($solicitud);

echo $respuesta;
```

## Inmersión Profunda:
La autenticación básica HTTP fue introducida en los años 90 como una forma simple de autenticar usuarios en aplicaciones web. Sin embargo, no es una forma segura de enviar contraseñas, ya que los datos son codificados en lugar de ser encriptados. Existen otras formas más seguras de autenticación, como la autenticación OAuth y la autenticación de dos factores. La autenticación básica también puede ser implementada en otros lenguajes de programación, no solo en PHP.

## Ver También:
- [Documentación de PHP sobre la autenticación básica HTTP] (https://www.php.net/manual/es/features.http-auth.php)
- [Explicación mas detallada sobre la autenticación básica HTTP] (https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)