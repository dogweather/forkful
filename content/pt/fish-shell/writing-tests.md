---
title:                "Fish Shell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Fish Shell?

Escrever testes é uma prática importante para garantir a qualidade do nosso código e evitar erros. No caso do Fish Shell, os testes podem nos ajudar a verificar se os nossos scripts estão funcionando corretamente e a detectar possíveis erros antes de colocá-los em produção.

## Como escrever testes em Fish Shell

Para escrever testes em Fish Shell, podemos usar a ferramenta integrada de teste chamada `fish -n`. Essa ferramenta é capaz de ler scripts de teste e executá-los, exibindo o resultado dos testes no terminal.
```
# Script de teste para verificar se o comando "ls" retorna o conteúdo correto
# Teste de sucesso se o conteúdo é igual a "meuArquivo.txt"
fish -n <<EOF
ls | grep -q "meuArquivo.txt"
if test \$status -ne 0
    echo "ERRO: Arquivo não encontrado."
    exit 1
end
EOF
```

No exemplo acima, usamos o comando `grep` para verificar se a saída do comando `ls` contém o arquivo "meuArquivo.txt". Caso não contiver, o teste falhará e exibirá uma mensagem de erro.

Também é possível usar a ferramenta `fish -c` para executar comandos diretamente no shell de teste, facilitando a verificação de resultados sem precisar criar um arquivo de teste separado.
```
# Executando o comando "echo" dentro do shell de teste
fish -c 'echo "Hello World"'
```

## Aprofundando-se nos testes em Fish Shell

Além da ferramenta `fish -n`, também é possível escrever testes com a ajuda da biblioteca [bats-core](https://github.com/bats-core/bats-core). Com ela, é possível criar testes mais complexos e com uma sintaxe mais amigável.

Outra dica importante é usar variáveis de ambiente para especificar caminhos ou configurações necessárias para a execução dos testes. Isso torna os testes mais dinâmicos e facilita a execução em diferentes ambientes.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/cmds/test.html)
- [Repositório do bats-core no GitHub](https://github.com/bats-core/bats-core)
- [Lista de comandos e sintaxe de teste no Fish Shell](https://fishshell.com/docs/current/cmds/test.html)