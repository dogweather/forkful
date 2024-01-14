---
title:                "C#: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Há várias razões pelas quais você pode querer enviar uma solicitação HTTP com autenticação básica. Por exemplo, pode ser necessário acessar dados protegidos por senha ou realizar ações específicas em um determinado servidor. Independentemente do motivo, é importante saber como fazer isso corretamente para garantir que sua solicitação seja processada corretamente.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em C#, você precisará seguir alguns passos simples. Primeiro, você precisará obter os dados de autenticação, como o nome de usuário e a senha do servidor. Em seguida, você pode criar um objeto de solicitação HTTP e adicioná-los aos cabeçalhos da solicitação. Aqui está um exemplo de código que mostra como fazer isso:

```C#
// Dados de autenticação
string username = "usuario";
string password = "senha";

// Cria objeto de solicitação HTTP com URL do servidor
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("https://meuservidor.com/api/dados");

// Adiciona dados de autenticação ao cabeçalho
string credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes(username + ":" + password));
request.Headers.Add("Authorization", "Basic " + credentials);

// Envia a solicitação e obtém a resposta
HttpWebResponse response = (HttpWebResponse)request.GetResponse();
```

Ao enviar a solicitação, você receberá uma resposta do servidor com o status da solicitação e os dados ou resultados solicitados.

## Deep Dive

Além dos passos mencionados acima, é importante entender um pouco mais sobre como as solicitações HTTP com autenticação básica funcionam. Quando você adiciona os dados de autenticação ao cabeçalho da solicitação, está criando um parâmetro de autorização usando o esquema "Basic". Isso significa que os dados de autenticação serão codificados em Base64 antes de serem enviados para o servidor. No entanto, é importante notar que esse processo não é considerado totalmente seguro, pois é possível decodificar os dados de autenticação do cabeçalho.

Outra coisa a ter em mente é que nem todos os servidores e APIs aceitam a autenticação básica. Alguns podem exigir métodos mais seguros, como OAuth ou tokens de acesso. Portanto, é sempre importante verificar as documentações antes de enviar suas solicitações.

## Veja também

- [Tutorial sobre como enviar solicitações HTTP em C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.httphttpclient)
- [Documentação de autenticação básica em servidores IIS](https://docs.microsoft.com/en-us/iis/configuration/system.webserver/security/authentication/basicAuthentication)
- [Tutorial sobre autenticação de API em C#](https://www.c-sharpcorner.com/article/authentication-using-web-api/)

Se você quiser aprender mais sobre o uso de autenticação básica em solicitações HTTP em C#, recomendamos que você confira esses recursos para obter mais informações e exemplos práticos. Boas codificações! 