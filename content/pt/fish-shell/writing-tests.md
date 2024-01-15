---
title:                "Escrevendo testes"
html_title:           "Fish Shell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes no Fish Shell?

Testes são uma parte crucial do processo de desenvolvimento de software. Eles permitem que os desenvolvedores encontrem e corrijam bugs antes que o código seja implantado em produção. No Fish Shell, escrever testes é particularmente importante porque ajuda a garantir que nossos scripts e comandos funcionem corretamente e evita surpresas indesejadas ao usá-los.

## Como fazer:

```Fish Shell
#!/usr/bin/fish

# Importar a biblioteca de testes
source tests.fish

# Definir uma função que será testada
function soma
    set a $argv[1]
    set b $argv[2]
    math "$a + $b"
end

# Chamar a função de teste
test "Soma de 2 + 3 deve ser igual a 5" "soma 2 3" -eq 5

# Executar todos os testes
run_tests
```

Ao rodar o script, o resultado será exibido em forma de tabela, indicando se os testes passaram ou falharam:

```
SUITE                             PASS FAIL
----                              ---- ----
Soma de 2 + 3 deve ser igual a 5  1    0
```

## Profundidade:

Ao escrever testes no Fish Shell, é importante entender alguns conceitos básicos:

- Os testes são escritos na forma de funções e podem ser chamados em qualquer lugar do script.
- A função `test` é usada para definir um teste, ela possui três argumentos: uma mensagem de descrição, o comando a ser testado e a condição de sucesso.
- A função `run_tests` é usada para executar todos os testes definidos no script.

Outra técnica importante é usar o comando `bt_run` para rodar testes rapidamente enquanto se está trabalhando em um script. Isso permite uma iteração mais eficiente e descobrir possíveis erros de forma mais ágil.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Como escrever testes automatizados eficientes no Fish Shell](https://spin.atomicobject.com/2016/11/29/shell-script-tested/)

Agora que você já sabe como escrever testes no Fish Shell, experimente e descubra como essa prática pode melhorar a qualidade dos seus scripts e comandos.