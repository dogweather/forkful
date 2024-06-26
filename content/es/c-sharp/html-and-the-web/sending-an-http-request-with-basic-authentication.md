---
date: 2024-01-20 18:01:25.700822-07:00
description: "C\xF3mo Hacerlo Primero, aseg\xFArate de tener `using System.Net;` y\
  \ `using System.Text;` en tu archivo. Aqu\xED hay un ejemplo de c\xF3mo enviar una\
  \ solicitud con\u2026"
lastmod: '2024-04-05T22:38:59.474237-06:00'
model: gpt-4-1106-preview
summary: "C\xF3mo Hacerlo Primero, aseg\xFArate de tener `using System.Net;` y `using\
  \ System.Text;` en tu archivo. Aqu\xED hay un ejemplo de c\xF3mo enviar una solicitud\
  \ con autenticaci\xF3n b\xE1sica."
title: "Enviando una solicitud http con autenticaci\xF3n b\xE1sica"
weight: 45
---

## Cómo Hacerlo
Primero, asegúrate de tener `using System.Net;` y `using System.Text;` en tu archivo. Aquí hay un ejemplo de cómo enviar una solicitud con autenticación básica:

```C#
using System;
using System.Net;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        var client = new HttpClient();
        var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("usuario:contraseña"));
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);
        
        try
        {
            var response = await client.GetAsync("http://tuservicio.com/dato");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("\nExcepción capturada!");
            Console.WriteLine($"Mensaje :{e.Message} ");
        }
    }
}
```
Si todo va bien, verás la respuesta del servidor en la consola. Si hay un error, imprimirá un mensaje de excepción.

## Profundizando
La autenticación básica HTTP es un sistema antiguo (inventado a principios de los años 90) para controlar el acceso a sitios web y servicios. Mientras es simple y ampliamente soportado, no es la más segura porque las credenciales se envían en texto claro, solo codificadas en Base64, que es fácil de decodificar.

Alternativas modernas:
- **OAuth**: Un estándar abierto para acceso delegado, usado comúnmente para permitir usuarios a iniciar sesión con servicios como Google o Facebook.
- **JWT**: Tokens Web JSON que son seguros y autónomos, pueden contener toda la información de la identidad del usuario.

Aspectos de Implementación:
- Usa `HttpClient` para peticiones: es eficiente y soporta operaciones asincrónicas.
- Siempre envía credenciales por HTTPS para evitar interceptaciones.
- Considera el manejo de excepciones y validaciones de respuesta.

## Ver También
- [Autenticación HTTP básica en MDN](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [Documentación de HttpClient en Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient?view=netframework-4.8)
- [Seguridad en la autenticación web en OWASP](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)
- [OAuth](https://oauth.net/)
- [JWT](https://jwt.io/introduction/)

Estos enlaces son un buen comienzo para entender mejor la autenticación HTTP básica y sus alternativas modernas.
