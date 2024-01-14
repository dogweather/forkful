---
title:                "Fish Shell: Escrevendo testes"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

# Por que escrever testes em Fish Shell?

Testes são importantes para garantir que seu código funcione corretamente e minimize erros em seu programa. Além disso, eles ajudam a ter uma melhor compreensão do seu código e facilitam a manutenção no futuro.

## Como escrever testes em Fish Shell

Para começar, é necessário instalar o módulo de teste Fisherman:

```Fish Shell
fisher add fisherman/test
```

Feito isso, é possível escrever seus próprios testes usando a função de teste integrada do Fish Shell. Por exemplo, vamos supor que temos uma função que realiza uma operação de soma:

```Fish Shell
function soma -a num1 -a num2
    echo $(math "$num1 + $num2")
end
```

Para testar essa função, podemos usar a função de teste integrada da seguinte maneira:

```Fish Shell
function teste-soma
    descreva "Teste para função soma"
    test num1 = 2
    test num2 = 3
    test (soma $num1 $num2) -eq 5
    resultado --erro no teste "A soma não está funcionando corretamente"
end
```

Na função de teste, usamos o comando "test" para verificar se as condições desejadas estão sendo atendidas e, em seguida, usamos a função "resultado" para especificar a mensagem de erro caso algum teste falhe.

## Mais informações sobre escrita de testes

Além do básico, existem outras funções e opções que podem ser usadas para escrever testes mais complexos e abrangentes. Alguns exemplos incluem:

- A função "test-all" que executa todos os testes dentro de uma função de teste.
- A função "esperando" que monitora uma variável e executa um teste quando ela atinge um determinado valor.
- O comando "-e" para executar comandos antes de cada teste.
- O comando "-s" para pular testes específicos.

Para saber mais sobre todas as opções disponíveis, você pode conferir a documentação completa do módulo de teste Fisherman ou explorar outras referências online.

# Veja também

- Documentação do módulo de teste Fisherman (https://github.com/fisherman/test)
- Tutorial em vídeo sobre testes em Fish Shell (https://www.youtube.com/watch?v=oKmPGCejvRM)
- Fórum de discussão sobre testes em Fish Shell (https://www.reddit.com/r/fishshell/comments/lmz0jc/testing_in_fish/)