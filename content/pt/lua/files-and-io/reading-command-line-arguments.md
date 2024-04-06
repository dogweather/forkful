---
date: 2024-01-20 17:56:20.435559-07:00
description: "Como Fazer: Sa\xEDda esperada se o nome foi fornecido."
lastmod: '2024-04-05T21:53:47.070148-06:00'
model: gpt-4-1106-preview
summary: "Sa\xEDda esperada se o nome foi fornecido."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
```Lua
-- salve o script como saudacao.lua
-- para rodar: lua saudacao.lua João

local nome = arg[1]  -- Captura o primeiro argumento da linha de comando
if nome then
    print("Olá, " .. nome .. "!")
else
    print("Olá, mundo!")
end
```
Saída esperada se o nome foi fornecido:
```
Olá, João!
```
Saída esperada se nenhum nome foi fornecido:
```
Olá, mundo!
```

## Detalhando
Historicamente, a habilidade de passar argumentos de linha de comando tem suas raízes nos primórdios da computação, onde a interação com programas era feita majoritariamente por texto. Lua, seguindo essa convenção, disponibiliza os argumentos através da tabela global `arg`.

Alternativas para leitura de argumentos podem incluir a solicitação de entrada do usuário durante a execução do programa ou lendo de um arquivo. Quanto aos detalhes de implementação, o índice `0` do `arg` contém o próprio script, e os índices positivos contam os argumentos passados, enquanto que os índices negativos contêm qualquer argumento de opção vindo antes do script na linha de comando.

## Veja Também
- Documentação oficial de Lua para os argumentos de linha de comando: https://www.lua.org/manual/5.4/manual.html#6.1
- Tutorial de Lua para iniciantes, que cobre os fundamentos: https://www.tutorialspoint.com/lua/index.htm
- Uma discussão sobre padrões de design de linha de comando em Lua: https://lua.space/general/cli-args-parsing-with-lua
