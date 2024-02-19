---
aliases:
- /pt/bash/sending-an-http-request/
date: 2024-01-20 17:59:17.887275-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP significa solicitar dados ou servi\xE7\
  os atrav\xE9s da internet. Programadores fazem isso para interagir com APIs, servi\xE7\
  os web, ou\u2026"
lastmod: 2024-02-18 23:08:58.322603
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP significa solicitar dados ou servi\xE7\
  os atrav\xE9s da internet. Programadores fazem isso para interagir com APIs, servi\xE7\
  os web, ou\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP"
---

{{< edit_this_page >}}

## O Que & Porquê?
Enviar uma requisição HTTP significa solicitar dados ou serviços através da internet. Programadores fazem isso para interagir com APIs, serviços web, ou simplesmente para automatizar a coleta de dados.

## Como Fazer:
Para enviar uma requisição HTTP do Bash, você pode usar o comando `curl`. Aqui estão alguns exemplos:

```Bash
# Envia uma requisição GET para a URL específica
curl https://api.exemplo.com/dados

# Envia uma requisição POST com alguns dados em formato JSON
curl -X POST -H "Content-Type: application/json" -d '{"chave":"valor"}' https://api.exemplo.com/enviar

# Salva a resposta da requisição em um arquivo
curl https://api.exemplo.com/dados -o dados_recebidos.txt
```

Veja um pequeno exemplo da saída para uma requisição GET simples:

```Bash
$ curl http://httpbin.org/get

{
  "args": {}, 
  "headers": {
    "Accept": "*/*", 
    "Host": "httpbin.org", 
    "User-Agent": "curl/7.68.0"
  }, 
  "origin": "203.0.113.195", 
  "url": "https://httpbin.org/get"
}
```

## Mergulho Profundo:
Enviar requisições HTTP é fundamental para a web moderna. O comando `curl` foi criado em 1996 e se tornou uma ferramenta onipresente para teste de APIs e automação de scripts. Como alternativa ao `curl`, você pode usar `wget`, que é habitualmente utilizado para baixar arquivos, mas menos flexível para enviar requisições variadas.

Detalhes de implementação como cabeçalhos HTTP e métodos de requisição (GET, POST, PUT, DELETE) são vitais. O `curl` permite modularizar esses detalhes, tornando-o poderoso e flexível. A segurança também é crítica; certifique-se de usar URLs `https` para encriptação e cuidado ao passar dados sensíveis via linha de comando.

## Veja Também:
Aprofunde-se em `curl` com:
- [Curl Documentation](https://curl.se/docs/)
- [HTTP request methods - MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- Para uma ferramenta com interface gráfica, experimente [Postman](https://www.postman.com/). 
- Se interessar em programação de scripts mais avançada, olhe para [Bash Scripting Guide](https://tldp.org/LDP/abs/html/).
