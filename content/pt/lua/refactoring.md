---
title:                "Refatoração"
date:                  2024-01-26T01:46:58.572681-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/refactoring.md"
---

{{< edit_this_page >}}

## O Quê & Por Que?
Refatoração é a arte de ajustar o código existente para melhorar sua estrutura, legibilidade e eficiência sem alterar seu comportamento externo. Programadores fazem isso para tornar seu código mais fácil de manter, reduzir complexidade e, muitas vezes, como um passo preliminar antes de adicionar novas funcionalidades ou corrigir bugs.

## Como fazer:
Vamos pegar uma função Lua simples e refatorá-la. Começamos com uma função que calcula a soma dos números em uma lista, mas é escrita sem muita preocupação com eficiência ou clareza:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Saída: 10
```

Refatore para uma versão mais eficiente e legível:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Ainda saída: 10
```

A versão refatorada elimina o loop interno redundante, usando `ipairs` para iterar pela lista de forma limpa.

## Aprofundamento
Historicamente, a refatoração vem da comunidade de programação Smalltalk no final dos anos 80 e foi popularizada pelo livro de Martin Fowler 'Refactoring: Improving the Design of Existing Code'. Em Lua, a refatoração geralmente envolve simplificar condicionais complexas, quebrar funções grandes em menores e otimizar o uso de tabelas para melhorar o desempenho.

A refatoração em Lua tem suas ressalvas; a natureza dinâmica de Lua e a tipagem flexível podem tornar certas refatorações, como renomear variáveis ou mudar assinaturas de funções, mais arriscadas se não forem feitas com cautela. Ferramentas para análise de código estático (como `luacheck`) podem diminuir tais riscos. Alternativas incluem o desenvolvimento dirigido por testes (TDD), onde o código é continuamente refatorado como parte integrante do processo de desenvolvimento, em contraste com uma fase de refatoração separada.

## Veja Também
- "Programming in Lua" por Roberto Ierusalimschy para melhores práticas e exemplos.
- "Refactoring: Improving the Design of Existing Code" por Martin Fowler para princípios aplicáveis em diversas linguagens.
- Diretório LuaRocks (https://luarocks.org/) para ferramentas e módulos voltados para a manutenção e refatoração de código Lua.
