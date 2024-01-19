---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Baixar uma página web é o processo de copiar e armazenar dados da Internet em um local de armazenamento local. Programadores fazem isso para análise de dados, coleta de informações ou automação de tarefas.

## Como Fazer:

Para baixar uma página web em Lua, usaremos o módulo `socket.http`. Se você não o tem, instale com: `luarocks install luasocket`.

```Lua
local http = require('socket.http')
local url = "http://example.com"

-- Fazendo a requisição HTTP
local response, status_code = http.request(url)

-- Imprimindo o resultado
if status_code == 200 then
  print(response)
else
  print("Falha! Código de status HTTP: " .. status_code)
end
```
Isso imprimirá o código HTML da página escolhida na tela.

## Mergulho Profundo

O protocolo HTTP tem sido a base da comunicação de dados na web desde a sua criação em 1991. Lua implementa esse protocolo no módulo `luasocket`, que é uma excelente opção para a tarefa devido a sua simplicidade e eficácia.

Uma alternativa seria usar o módulo `luasec` se precisar de alguma interação segura com páginas HTTPS.

Um ponto a considerar na implementação é que esta solução síncrona pode bloquear a execução principal do programa ao baixar página web. Por isso, pode ser vantajoso baixar a página em uma thread separada se sua aplicação for sensível a longos períodos de tempo de bloqueio.

## Veja Também:

1. Documentação do módulo `luasocket`: http://w3.impa.br/~diego/software/luasocket
2. Documentação do módulo `luasec`: https://github.com/brunoos/luasec/wiki
3. Discussão sobre threads em Lua: https://www.lua.org/pil/9.html