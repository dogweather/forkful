---
title:                "Bash: Escrevendo testes"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Bash?

A programação em Bash pode ser divertida e produtiva, mas também pode ser desafiadora. Às vezes, você escreve um script que parece funcionar perfeitamente, mas quando acaba de executá-lo, percebe que algo está errado. Para evitar esse tipo de frustração, é importante escrever testes para o seu código em Bash. Eles garantem que seu script funcione corretamente e também facilitam a identificação e correção de erros.

## Como escrever testes em Bash

Para escrever testes em Bash, você pode usar a estrutura do comando `test`. Ele permite que você verifique condições lógicas e gere saídas diferentes com base nos resultados dos testes. Por exemplo:

```Bash
#!/bin/bash

variavel=10

# Testa se a variável é maior que 5
if [ $variavel -gt 5 ]; then
  echo "A variável é maior que 5"
else
  echo "A variável é menor que 5"
fi
```

Neste exemplo, a saída seria "A variável é maior que 5", pois o valor da variável é maior que 5. Experimente alterar o valor da variável e verá que a saída também muda.

Outra opção é usar o comando `test` diretamente, seguido de um operador lógico e um valor a ser testado. Por exemplo:

```Bash
#!/bin/bash

variavel=10

# Testa se a variável é maior que 5
if test $variavel -gt 5; then
  echo "A variável é maior que 5"
else
  echo "A variável é menor que 5"
fi
```

Os operadores lógicos mais comuns são `-eq` (igual a), `-ne` (diferente de), `-gt` (maior que), `-lt` (menor que), `-ge` (maior ou igual a) e `-le` (menor ou igual a). Você pode combinar esses operadores usando `&&` para "e" e `||` para "ou".

## Deep Dive

Além dos testes básicos, existem outras técnicas para escrever testes mais avançados em Bash. Você pode usar a ferramenta `BATS` (Bash Automated Testing System) para tornar a criação e execução de testes mais fácil e eficiente. Ele tem uma sintaxe semelhante ao `bash`, mas inclui recursos extras, como configuração e relatórios de teste.

Você também pode usar a ferramenta `catch` para testar a saída de cada etapa do seu script em Bash. Isso permite que você detecte com facilidade onde os erros ocorrem e os corrija rapidamente.

O teste de unidade é outra técnica importante para garantir que seu código em Bash esteja funcionando corretamente. Ele consiste em testar apenas uma parte específica do seu código e isolar quaisquer problemas que possam surgir nessa área.

## Veja também

- [Tutorial de testes em Bash](https://www.shellscript.sh/testing.html)
- [Documentação do comando test](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [BATS - Bash Automated Testing System](https://github.com/sstephenson/bats)
- [Catch - ferramenta de teste para Bash](https://github.com/sharkdp/catch)