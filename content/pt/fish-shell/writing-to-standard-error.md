---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever no erro padrão, ou 'stderr', é a prática de mandar mensagens de erro ou diagnóstico de um programa para um canal específico, separado da saída de dados principal ou 'stdout'. Programadores utilizam isso para facilitar o debug e garantir que as mensagens de erro possam ser tratadas ou registradas adequadamente.

## Como Fazer:
```Fish Shell
# Escrevendo uma mensagem de erro para stderr
echo "Erro: arquivo não encontrado" >&2

# Exemplo com stderr e stdout
echo "isso vai para o stdout"
echo "isso vai para o stderr" >&2
```
Saída esperada:
```
isso vai para o stdout
Erro: arquivo não encontrado # Esta linha vai para o stderr
isso vai para o stderr       # Esta também vai para o stderr
```

## Aprofundando:
Historicamente, a separação do stdout e stderr permite que os programas comuniquem eficientemente o resultado da execução e os possíveis erros que ocorram. Alternativeamente, é possível redirecionar o stderr para um arquivo (`2> arquivo.log`) ou para o stdout (`2>&1`), dependendo da necessidade de capturar os erros para análise posterior ou para combinar ambos os fluxos. Ao implementar, é importante compreender que o stderr é normalmente usado síncrono, reduzindo as chances de mensagens misturadas e tornando mais simples a depuração.

## Veja Também:
- Documentação oficial do Fish Shell sobre redirecionamentos: https://fishshell.com/docs/current/index.html#redirections
- Tutorial sobre fluxos de saída em shells UNIX: https://www.gnu.org/software/bash/manual/html_node/Redirections.html
- Guia para entender e usar o stdout e stderr no Linux: https://tldp.org/LDP/abs/html/io-redirection.html
