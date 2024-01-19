---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Criar um arquivo temporário é um processo no qual um programa cria um arquivo com dados temporários para fins de trabalho. Programadores fazem isso para evitar preencher a memória do computador com dados que só são necessários durante a execução do programa atual.

## Como fazer:

``` Bash
# Usando o comando mktemp
arquivo_temp=$(mktemp)
echo "Este é um arquivo temporário" > $arquivo_temp
cat $arquivo_temp
```

Saída:

``` Bash
Este é um arquivo temporário
```

## Aprofundando

Arquivos temporários foram primeiro implementados no Unix versão 7, no final dos anos 70. Uma alternativa ao mktemp pode ser a implementação manual de sequências de caracteres aleatórios para gerar nomes de arquivo únicos, mas isso pode ser menos seguro.

Detalhes da implementação do mktemp incluem a criação de um arquivo com permissões estritamente limitadas (apenas o usuário que o criou pode ler ou escrever nele), e gera um nome de arquivo que é garantido ser único.

## Veja também:

- Manual do mktemp: https://man7.org/linux/man-pages/man1/mktemp.1.html
- Mais detalhes sobre a implementação do mktemp: https://www.gnu.org/software/autogen/mktemp.html
- Informações úteis relacionadas à manipulação de arquivos: https://www.guru99.com/linux-regular-expressions.html