---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Download de uma página da web com C#

## O Que & Porquê?

Fazer download de uma página da web é basicamente obter o código-fonte HTML para análise ou processamento posterior. Os programadores fazem isto para scrappear dados, testar interfaces e implementar bots automatizados.

## Como fazer:

Se estivermos a usar o `HttpClient`, um código simples poderia ser:

```C#
using (var cliente = new HttpClient())
{
    var html = await cliente.GetStringAsync("http://google.com");
    Console.WriteLine(html);
}
```

Este é o output:

```C#
<!doctype html>...
```

Se utilizarmos o `WebClient`:

```C#
using (var cliente = new WebClient())
{
    cliente.Encoding = System.Text.Encoding.UTF8;
    var html = cliente.DownloadString("http://google.com");
    Console.WriteLine(html);
}
```
A saída será semelhante:

```C#
<!doctype html>...
```

## Aprofundando

A técnica de download de páginas da web tem sido usada desde os primeiros dias da internet, com uma série de métodos e bibliotecas a serem introduzidos ao longo do tempo para facilitar o processo.

Existem diversas alternativas ao `HttpClient` e ao `WebClient`, incluindo `HttpWebRequest`, `RestSharp`, `Flurl` e muitos outros. A escolha depende de fatores como as necessidades específicas do projeto, as preferências pessoais, a experiência anterior, etc.

No que diz respeito à implementação, ao usar `HttpClient` ou `WebClient` estamos a enviar um pedido HTTP GET para o servidor. O servidor então responde com o HTML da página que o nosso código posteriormente pode analisar e processar.

## Veja também

Para mais informações consulte os seguintes links:
- [HttpClient na Microsoft Docs](https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [WebClient na Microsoft Docs](https://docs.microsoft.com/pt-br/dotnet/api/system.net.webclient?view=net-5.0)