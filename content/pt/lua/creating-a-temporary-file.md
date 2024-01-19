---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

A criação de um arquivo temporário em programação é uma prática comum usada para armazenar dados temporariamente durante a execução do programa. Isso pode ser útil em situações onde você precisa manipular uma grande quantidade de dados sem consumir uma quantidade significativa de memória.

## Como Fazer:

Aqui está um exemplo de como criar um arquivo temporário em Lua.

```Lua
local tmp = os.tmpname()
local fp = io.open(tmp, "w")

fp:write("Testando o arquivo temporário em Lua")
fp:close()

fp = io.open(tmp, "r")
print(fp:read())
fp:close()

os.remove(tmp)
```

Na execução, o script acima irá imprimir `Testando o arquivo temporário em Lua`.

## Aprofundando:

Historicamente, arquivos temporários têm sido usados em duas ocasiões principais: manipulações de grande quantidade de dados e operações de IO (input/output). Lua, com a sua eficiente manipulação de strings a apenas um comando de distância, possibilita um espaçoso campo para que arquivos temporários possam ser usados em larga escala.

Existe uma função alternativa em Lua, `io.tmpfile()`, que cria um arquivo temporário que é automaticamente removido quando o programa termina. No entanto, este arquivo não tem um nome de arquivo válido e não pode ser aberto por outro programa durante a execução do programa Lua.

Quanto à implementação, quando `os.tmpname()` é chamado, Lua cria um nome único para o arquivo temporário. O arquivo é então aberto, escrito, lido e, finalmente, removido usando `os.remove()`.

## Veja Também:

Para mais detalhes sobre os tópicos discutidos, veja os seguintes links:

- Manipulação de arquivos em Lua: https://www.lua.org/pil/21.2.2.html
- Funções de entrada/saída: https://www.lua.org/pil/21.1.html
- Documentação oficial de Lua, com referência completa: https://www.lua.org/docs.html