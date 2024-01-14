---
title:    "Bash: Escrevendo testes"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que escrever testes?

Escrever testes é uma parte essencial do desenvolvimento de software. Com testes bem escritos, podemos garantir que nosso código esteja funcionando corretamente e prevenir possíveis bugs. Além disso, testes também ajudam a documentar o comportamento esperado de nossas funções e tornam o processo de depuração mais fácil.

## Como escrever testes em Bash

Escrever testes em Bash é simples e envolve o uso de comando `test` para verificar se uma determinada condição é verdadeira ou falsa. Aqui está um exemplo de como testar se uma variável é igual a um determinado valor:

```Bash
#!/bin/bash

# Definindo variável
nome="Pedro"

# Testando se a variável é igual a "Pedro"
if [ $nome = "Pedro" ]; then
    echo "Variável 'nome' é igual a Pedro"
fi
```

No código acima, usamos o operador de igualdade `=` dentro do comando `test` para verificar se a variável `nome` é igual a "Pedro". Caso a condição seja verdadeira, o código dentro do `if` será executado e a mensagem será impressa na tela.

Além disso, também podemos usar outras opções, como `-n` para verificar se uma variável não está vazia ou `-e` para verificar se um arquivo existe. Consulte a documentação do `test` para obter mais informações sobre os operadores e opções disponíveis.

## Aprofundando em escrever testes

Uma boa prática ao escrever testes em Bash é usar o comando `set -e` no início do seu script. Isso garantirá que o script pare imediatamente caso algum comando falhe, o que ajuda a detectar e corrigir erros de forma mais rápida.

Além disso, também é possível usar a estrutura `if/else` para testar diferentes cenários e garantir que nosso código esteja lidando com situações imprevistas de forma adequada.

Não se esqueça de também testar seu código manualmente antes de implementar os testes automatizados e sempre atualize os testes quando fizer alterações no seu código.

## Veja também

- Documentação do comando `test`: https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html
- Tutorial sobre testes em Bash: https://www.baeldung.com/linux/bash-test-equivalent