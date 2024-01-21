---
title:                "Lendo argumentos da linha de comando"
date:                  2024-01-20T17:56:20.435559-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Ler argumentos de linha de comando em Lua significa pegar dados inseridos diretamente quando você executa seu script. Programadores fazem isso para tornar seus programas flexíveis, permitindo que usuários especifiquem comportamentos ou parâmetros de execução diretos do terminal.

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