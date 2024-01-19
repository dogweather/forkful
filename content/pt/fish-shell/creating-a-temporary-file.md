---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Criar um arquivo temporário implica em criar um arquivo de armazenamento curto que desaparece após a sessão do usuário acabar. Os programadores o usam para armazenar dados transacionalmente durante a execução de um programa, por exemplo, ao manipular grandes volumes de dados cuja persistência não é necessária.

## Como usar:

No Shell do Fish, a criação de um arquivo temporário é simples. Aqui está um exemplo de código:

```fish
set tmpfile (mktemp)
echo "Isto é um arquivo temporário!" > $tmpfile
cat $tmpfile
rm $tmpfile
```

E aqui está a saída do código acima, assumindo que `mktemp` retorna `/tmp/tmp.1F2G3H`.

```fish
Isto é um arquivo temporário!
```

## Mergulho profundo:

O conceito de arquivos temporários surgiu como uma maneira de permitir que os programas manipulassem grandes volumes de dados que não precisariam ser mantidos de forma contínua.

Existem alternativas ao uso de arquivos temporários. Por exemplo, alguns programadores podem preferir usar Bancos de Dados em Memória, como o Redis. No entanto, para conjuntos de dados particularmente grandes onde a memória pode ser uma preocupação, os arquivos temporários geralmente são a melhor solução.

A implementação dos arquivos temporários no Fish Shell de fato utiliza a ferramenta padrão UNIX `mktemp`, que cria um arquivo único em /tmp. A supressão do arquivo é responsabilidade do programador.

## Veja também:

Por favor, revise as seguintes fontes para mais informações:

1. Documentação oficial do Fish Shell: [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
2. Mais informações sobre `mktemp`: [man mktemp](https://man7.org/linux/man-pages/man1/mktemp.1.html)
3. Alternativas de Banco de Dados em Memória: [Redis](https://redis.io/)

## Conclusão:

Usem arquivos temporários com sabedoria!