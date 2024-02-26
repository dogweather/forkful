---
date: 2024-01-20 18:02:26.935957-07:00
description: "Enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica \xE9\
  \ essencialmente fornecer nome de usu\xE1rio e senha para acessar recursos protegidos\
  \ em um servidor.\u2026"
lastmod: '2024-02-25T18:49:44.415506-07:00'
model: gpt-4-1106-preview
summary: "Enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica \xE9\
  \ essencialmente fornecer nome de usu\xE1rio e senha para acessar recursos protegidos\
  \ em um servidor.\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O que é & Por que?
Enviar uma solicitação HTTP com autenticação básica é essencialmente fornecer nome de usuário e senha para acessar recursos protegidos em um servidor. Programadores fazem isso para interagir com APIs ou serviços web que requerem autenticação para uso.

## Como Fazer:
```PowerShell
# Defina as credenciais de usuário e senha
$credenciais = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("usuario:senha"))

# Crie o cabeçalho de autenticação
$headers = @{
    Authorization = "Basic $credenciais"
}

# Envie a solicitação GET com as credenciais
$resposta = Invoke-RestMethod -Uri "http://servidor.com/recurso" -Method Get -Headers $headers

# Exiba a resposta
$resposta
```

Saída de amostra (exemplo):
```PowerShell
Id     : 123
Nome   : Exemplo
Status : Ativo
```

## Mergulho Profundo
Enviar solicitações HTTP com autenticação básica remonta aos primeiros dias da web, uma forma simples, mas não a mais segura, de controle de acesso. Hoje, alternativas como OAuth 2.0 e tokens JWT são frequentemente recomendadas por oferecerem mais segurança. Na implementação, é crucial usar HTTPS para proteger as credenciais em trânsito. Além disso, a função `Invoke-RestMethod` do PowerShell abstrai a complexidade da formação da solicitação e manipulação da resposta, permitindo foco maior na lógica da aplicação.

## Veja Também
- Documentação oficial do PowerShell para [`Invoke-RestMethod`](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- Tutorial sobre autenticação HTTP básica na [MDN Web Docs](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
- Visão geral sobre segurança em APIs com [OAuth 2.0](https://oauth.net/2/) e [JWT (Json Web Tokens)](https://jwt.io/introduction/)
