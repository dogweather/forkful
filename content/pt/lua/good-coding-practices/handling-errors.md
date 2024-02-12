---
title:                "Tratamento de erros"
aliases:
- /pt/lua/handling-errors.md
date:                  2024-01-26T00:55:27.883879-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/handling-errors.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Tratar erros na codificação é esperar pelo inesperado. É a arte de se planejar para quando as coisas saírem do controle, para que você possa manter seu programa funcionando suavemente.

## Como Fazer:
O Lua utiliza duas funções principais para tratamento de erros: `pcall` e `xpcall`. Veja como usá-las:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Ops! Algo deu errado.")
    else
        print("Tudo certo!")
    end
end

-- Usando pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Sucesso!")
else
    print("Erro capturado:", errorMessage)
end

-- Usando xpcall com um manipulador de erro
function myErrorHandler(err)
    print("Manipulador de Erro diz:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("A chamada foi bem-sucedida?", status)
```

A saída de exemplo pode ser:

```
Erro capturado: Ops! Algo deu errado.
Manipulador de Erro diz: Ops! Algo deu errado.
A chamada foi bem-sucedida? false
```
Ou, se nenhum erro ocorrer:
```
Tudo certo!
Sucesso!
Tudo certo!
A chamada foi bem-sucedida? true
```

## Aprofundando
Tratar erros, ou "tratamento de exceções", nem sempre foi uma coisa. Os primeiros programas travavam – muito. Com a evolução da codificação, veio também a necessidade de estabilidade. A abordagem do Lua é simples se comparada com a de algumas linguagens. Não há blocos `try/catch`, apenas `pcall` e `xpcall`. O primeiro protege uma chamada de função, retornando um status e qualquer erro. O segundo adiciona uma função de tratamento de erros, útil para limpeza personalizada ou registro.

Uma alternativa no Lua é usar `assert`, que pode servir a um propósito semelhante, lançando um erro se a sua condição for falsa. No entanto, não é tão flexível quanto o `pcall` para cenários complexos de tratamento de erros.

Internamente, `pcall` e `xpcall` funcionam configurando um "ambiente protegido" para a função executar. Se um erro aparecer, o ambiente captura e pode ou tratar imediatamente ou passá-lo de volta para o programa lidar com ele.

## Veja Também
- O livro Programação em Lua (terceira edição), disponível em https://www.lua.org/pil/ para leitura detalhada sobre tratamento de erros (Seção 8.4).
- Manual de Referência Oficial do Lua 5.4: https://www.lua.org/manual/5.4/ - para as informações mais atualizadas sobre as funções de tratamento de erros do Lua.
- Wiki de usuários do Lua sobre tratamento de erros: http://lua-users.org/wiki/ErrorHandling – para insights da comunidade e padrões.
