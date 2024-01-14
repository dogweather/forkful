---
title:    "Bash: Excluindo caracteres que correspondem a um padrão"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existem algumas razões pelas quais alguém pode querer excluir caracteres que correspondem a um padrão em um código Bash. Por exemplo, pode ser necessário remover informações sensíveis, como senhas ou chaves de acesso, de um arquivo ou script.

## Como fazer

Para excluir caracteres de um arquivo que correspondem a um padrão, podemos usar a ferramenta de linha de comando `sed` em conjunto com expressões regulares. Por exemplo, se quisermos excluir todas as ocorrências de "senha" em um arquivo chamado "config.txt", podemos usar o seguinte comando:

```Bash
sed -i 's/senha//g' config.txt
```

Isso substituirá todas as ocorrências da palavra "senha" por uma string vazia, resultando na exclusão desses caracteres. O `-i` no comando garante que as alterações sejam feitas diretamente no arquivo, em vez de apenas imprimi-las no terminal.

Podemos usar expressões regulares mais complexas para corresponder a caracteres específicos ou padrões de palavras. Por exemplo, para excluir todas as senhas que começam com "senha" e terminam com um número, podemos usar o seguinte comando:

```Bash
sed -i 's/senha[0-9]//g' config.txt
```

## Mergulho profundo

A ferramenta `sed` é uma poderosa ferramenta de edição de texto que aceita expressões regulares para realizar tarefas como a exclusão de caracteres que correspondem a um padrão. As expressões regulares são uma forma de representar padrões de texto em uma sequência de caracteres. Elas são usadas em muitas linguagens de programação e ferramentas de linha de comando para realizar operações de busca e substituição.

No exemplo acima, usamos a expressão regular `[0-9]` para representar qualquer número de 0 a 9. Isso nos permitiu excluir todas as senhas que terminam com um número, independentemente de qual seja esse número.

Existem muitas outras expressões regulares que podem ser usadas com a ferramenta `sed` para excluir caracteres ou padrões específicos de um arquivo. É importante entender bem as expressões regulares para aproveitar ao máximo essa ferramenta de edição de texto.

## Veja também

- [Documentação do `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial de expressões regulares para Bash](https://www.regular-expressions.info/posixbrackets.html)
- [Outras ferramentas de linha de comando úteis para edição de texto](https://www.cyberciti.biz/howto/question/linux/find-replace-string-sed-awk-perl.php)