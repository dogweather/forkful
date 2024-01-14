---
title:    "Bash: Convertendo uma string para minúsculas."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Converter uma string para letras minúsculas pode ser uma tarefa útil ao trabalhar com strings em um programa Bash. Ao converter as letras para minúsculas, você pode facilmente comparar strings, fazer operações de busca e substituição, entre outras coisas.

## Como Fazer

Existem várias maneiras de converter uma string para minúsculas usando Bash. Aqui estão alguns exemplos usando diferentes comandos do Bash:

```Bash
# Exemplo 1: Usando o comando 'tr'
texto="HELLO WORLD"
echo $texto | tr '[:upper:]' '[:lower:]'
# Output: hello world

# Exemplo 2: Usando o comando 'awk'
texto="HELLO WORLD"
echo $texto | awk '{print tolower($0)}'
# Output: hello world

# Exemplo 3: Usando o comando 'sed'
texto="HELLO WORLD"
echo $texto | sed 's/.*/\L&/'
# Output: hello world
```

Como podemos ver nos exemplos acima, cada comando possui sua própria sintaxe para converter uma string para minúsculas. Mas o resultado será o mesmo em todos os casos.

## Deep Dive

Ao converter uma string para minúsculas em um programa Bash, é importante ter em mente que o idioma padrão do sistema é levado em consideração. Por exemplo, se seu sistema estiver configurado para o idioma inglês, a conversão para minúsculas será feita de acordo com as regras desse idioma.

Outro ponto a se considerar é que a conversão afeta apenas as letras e não outros caracteres, como acentos ou símbolos especiais.

Além disso, existem comandos adicionais do Bash que podem ser usados ​​para alterar o caso de uma string, como `echo ${texto,,}` ou `${texto,,}`. Esses comandos também podem ser úteis em determinadas situações, mas nem sempre são compatíveis com todas as versões do Bash.

Em resumo, converter uma string para minúsculas é uma tarefa relativamente simples, mas é importante conhecer as diferentes opções disponíveis para escolher a melhor para o seu programa.

## Veja também

- [Documentação do Bash sobre o comando 'tr'](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Documentação do Bash sobre o comando 'awk'](https://www.gnu.org/software/gawk/manual/html_node/Changing-Case.html)
- [Documentação do Bash sobre o comando 'sed'](https://www.gnu.org/software/sed/manual/html_node/Using-a-regex_002dreplace-command.html#Using-a-regex_002dreplace-command)