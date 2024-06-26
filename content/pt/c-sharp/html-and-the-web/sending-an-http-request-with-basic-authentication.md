---
date: 2024-01-20 18:01:04.191292-07:00
description: "Como Fazer: A autentica\xE7\xE3o b\xE1sica \xE9 um dos modos mais antigos\
  \ de autentica\xE7\xE3o em HTTP, embora simples, n\xE3o \xE9 o mais seguro. A senha\
  \ e o usu\xE1rio s\xE3o\u2026"
lastmod: '2024-04-05T21:53:46.927635-06:00'
model: gpt-4-1106-preview
summary: "A autentica\xE7\xE3o b\xE1sica \xE9 um dos modos mais antigos de autentica\xE7\
  \xE3o em HTTP, embora simples, n\xE3o \xE9 o mais seguro."
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

## Como Fazer:
```C#
using System;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

public class HttpRequestWithBasicAuth
{
    public static async Task Main(string[] args)
    {
        using (var client = new HttpClient())
        {
            var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("usuario:senha"));
            client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);

            try
            {
                var response = await client.GetAsync("http://seuservidor.com/recurso");
                response.EnsureSuccessStatusCode(); 
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nExceção capturada!");
                Console.WriteLine("Mensagem :{0} ", e.Message);
            }
        }
    }
}
```

Output:
```
{ "nome": "Exemplo", "descricao": "Resposta de um recurso protegido." }
```

## Deep Dive
A autenticação básica é um dos modos mais antigos de autenticação em HTTP, embora simples, não é o mais seguro. A senha e o usuário são enviados em texto base64, que pode ser facilmente decodificado. Por isso, é imprescindível usar sempre HTTPS quando se opta por esse método.

Alternativas mais seguras incluem autenticação Digest, OAuth e tokens de acesso JWT (JSON Web Token). No entanto, a autenticação básica ainda é comum para scripts internos ou em casos onde a simplicidade é prioritária e a segurança não é uma grande preocupação.

Detalhes de implementação importantes incluem a forma como as credenciais são armazenadas e manipuladas. Deve-se evitar manter informações sensíveis no código-fonte e considerar a utilização de variáveis de ambiente ou serviços de gerenciamento de segredos.

## Veja Também
- Documentação da Microsoft sobre o `HttpClient`: https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient
- OWASP sobre autenticação básica: https://owasp.org/www-community/controls/Basic_Authentication
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
