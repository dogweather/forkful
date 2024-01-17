---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "PowerShell: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que é e por que você precisa de enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP é uma parte importante de muitos processos de programação, e às vezes essa solicitação requer autenticação básica. Isso significa que você precisa fornecer um nome de usuário e senha para acessar um recurso protegido. Programadores fazem isso para garantir a segurança e a proteção dos dados em seus aplicativos.

## Como fazer:

Para enviar uma solicitação HTTP com autenticação básica no PowerShell, você pode usar o Cmdlet ```Invoke-RestMethod``` combinado com o parâmetro ```Credential```. Aqui está um exemplo de código que mostra como fazer isso:

```
$url = "http://exemplo.com/api/endpoint"
$username = "usuario"
$password = "senha"
$cred = New-Object System.Management.Automation.PSCredential($username, $password)
$response = Invoke-RestMethod -Uri $url -Credential $cred
```

O comando acima cria uma nova credencial com o nome de usuário e senha fornecidos e as passa como parâmetro para o Cmdlet ```Invoke-RestMethod```, que faz a solicitação HTTP com a autenticação básica incluída.

Se a solicitação for bem-sucedida, o valor de $response conterá a resposta do servidor. Você pode então manipular esses dados da maneira desejada.

## Mergulho profundo:

A autenticação básica é um mecanismo de segurança antigo e não é considerada muito segura, pois as credenciais são enviadas em texto simples. No entanto, ainda é amplamente usado em muitos sistemas legados e é fácil de implementar no PowerShell.

Existem outras formas de autenticação mais seguras, como o OAuth, que é usado por muitas APIs populares, mas isso requer um pouco mais de trabalho para implementar no PowerShell.

Ao enviar uma solicitação HTTP com autenticação básica, é importante garantir que a conexão é feita através de HTTPS, para que os dados sejam criptografados durante a transmissão.

## Veja também:

Para mais informações sobre o Cmdlet Invoke-RestMethod, consulte [a documentação oficial do Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1).

Para aprender mais sobre os diferentes tipos de autenticação disponíveis em HTTP, veja [este artigo da MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication).