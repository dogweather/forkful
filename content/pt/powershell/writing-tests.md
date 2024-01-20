---
title:                "Escrevendo testes"
html_title:           "PowerShell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## O que e por que?

Ao escrever código, os programadores devem garantir que o mesmo seja eficiente, funcional e livre de erros. A escrita de testes é uma técnica utilizada para validar o código e garantir que ele atenda aos requisitos esperados.

## Como fazer:

Para escrever testes em PowerShell, é necessário utilizar o módulo Pester, que é projetado especificamente para esse propósito. O Pester permite criar testes unitários, integração e funcionalidades, bem como relatar resultados detalhados.

```PowerShell
Describe 'Minha função teste' {
    It 'verifica se a saída é a esperada' {
        $variavel = 5 + 3
        $variavel | Should Be 8
    }
}
```

A saída do teste será exibida no formato de pontos verdes (```.```) se for bem-sucedido ou pontos vermelhos (```F```) se falhar.

## Aprofundando:

A prática de escrever testes remonta ao final do século XX, quando programadores começaram a reconhecer a importância de ter um processo de teste rigoroso para garantir a qualidade do código. Além do Pester, existem outras ferramentas disponíveis para testes em PowerShell, como o NUnit e o RSpec.

O Pester também oferece recursos de "mocking", que permitem simular os resultados de funções e comandos para testar diferentes cenários de maneira controlada. Além disso, os testes podem ser executados automaticamente sempre que houver uma nova versão do código ou durante integração contínua.

## Veja também:

- [Documentação do Pester](https://pester.dev/)
- [Vídeo sobre a importância de testes em PowerShell](https://www.youtube.com/watch?v=HXbZPHXgfYM)