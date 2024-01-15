---
title:                "Removendo caracteres correspondentes a um padrão"
html_title:           "Bash: Removendo caracteres correspondentes a um padrão"
simple_title:         "Removendo caracteres correspondentes a um padrão"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já enfrentou a tarefa de ter que trabalhar com texto em um arquivo usando o Bash. E muitas vezes, esse texto pode não estar exatamente da maneira que você precisa. É aí que entra a função de deletar caracteres que correspondem a um padrão. Ela permite que você remova partes do texto que não são relevantes para o que você está procurando ou precisa. Isso torna mais fácil e rápido trabalhar com os dados em um arquivo, economizando seu tempo e esforço.

## Como fazer

A sintaxe para excluir caracteres que correspondem a um padrão é simples. Tudo o que você precisa é do comando `sed` seguido pelos comandos `s/padrão/ /g` dentro das aspas. Por exemplo, se quisermos excluir todos os números em um determinado arquivo, podemos usar o seguinte comando:
```Bash
sed 's/[0-9]//g' arquivo.txt 
```
Isso irá substituir todos os números no arquivo pelo espaço em branco, efetivamente excluindo-os. Você também pode usar expressões regulares para refinar o seu padrão e tornar mais específico o que deve ser excluído. Ou, se você quiser manter alguns caracteres e excluir apenas outros, você pode usar o operador de negação `^` dentro do padrão. Por exemplo, `s/[^a-zA-Z]//g` irá manter apenas letras e excluir todos os outros caracteres.

## Profundidade

Se você quiser se aprofundar ainda mais no uso do `sed` para excluir caracteres que correspondem a um padrão, há muitas outras formas de personalizar seu comando. Por exemplo, você pode usar flags como `i` para ignorar maiúsculas e minúsculas ou `n` para suprimir o output. Você também pode combinar vários padrões em uma única linha, separando-os por ponto e vírgula (;). Ou, se quiser excluir um caractere ou conjunto de caracteres específicos, você pode usar `tr` em vez de `sed`. É sempre uma boa ideia consultar a documentação ou procurar exemplos de uso antes de tentar algo mais complexo.

## Veja também

- [Documentação do `sed`](https://linux.die.net/man/1/sed)
- [Exemplos de uso do `sed`](https://likegeeks.com/linux-sed-command/)