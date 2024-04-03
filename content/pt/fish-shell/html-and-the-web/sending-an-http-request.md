---
date: 2024-01-20 17:59:33.247990-07:00
description: "Como Fazer: No Fish Shell, voc\xEA pode usar `curl` ou outras ferramentas\
  \ como `httpie` para fazer requisi\xE7\xF5es HTTP. Aqui est\xE1 um jeito r\xE1pido\
  \ com `curl`."
lastmod: '2024-03-13T22:44:47.002911-06:00'
model: gpt-4-1106-preview
summary: "No Fish Shell, voc\xEA pode usar `curl` ou outras ferramentas como `httpie`\
  \ para fazer requisi\xE7\xF5es HTTP."
title: "Enviando uma requisi\xE7\xE3o HTTP"
weight: 44
---

## Como Fazer:
No Fish Shell, você pode usar `curl` ou outras ferramentas como `httpie` para fazer requisições HTTP. Aqui está um jeito rápido com `curl`:

```Fish Shell
# GET request para obter dados
curl 'https://api.exemplo.com/dados'

# POST request para enviar dados
curl -X POST 'https://api.exemplo.com/enviar' -d 'nome=Joao&cidade=Lisboa'

# Exemplo de resposta para um GET request
{
  "id": 123,
  "nome": "João",
  "cidade": "Lisboa"
}
```

## Mergulho Profundo:
Enviar requisições HTTP é vital desde o início da web. Apesar do `curl` ser amplamente usado hoje em dia, começou em 1997. Alternativas ao `curl` incluem `httpie`, `wget`, e ferramentas gráficas como Postman. É essencial entender os métodos HTTP como GET, POST, PUT, DELETE, dentre outros, cada um com um propósito específico. Em termos de implementação, a ferramenta escolhida envia um pedido por meio do protocolo HTTP, que o servidor interpretará e responderá de acordo com a lógica e dados disponíveis.

## Veja Também:
- Documentação do `curl`: https://curl.se/docs/
- Comparação entre `curl` e `httpie`: https://httpie.io/docs#comparison-with-curl
- Guia de métodos HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Methods
- Postman para testar APIs: https://www.postman.com/
