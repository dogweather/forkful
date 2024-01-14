---
title:                "Bash: Maiúscula de uma sequência de caracteres"
simple_title:         "Maiúscula de uma sequência de caracteres"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Bash?

Capitalizar uma string significa deixar a primeira letra de cada palavra maiúscula, seguindo a gramática padrão da língua portuguesa. Isso pode ser útil em situações onde é necessário garantir a formatação correta do texto, como em nomes próprios ou títulos de documentos.

## Como capitalizar uma string em Bash?

Para capitalizar uma string em Bash, podemos usar o comando `tr`. Veja um exemplo de código abaixo:

```
# Criando uma variável com uma string
texto="essa é uma string de teste"

# Usando o comando tr para capitalizar a string
nova_string=$(echo $texto | tr '[:lower:]' '[:upper:]')

# Imprimindo a nova string capitalizada
echo $nova_string
```

Neste exemplo, usamos o comando `tr` para transformar todas as letras minúsculas para maiúsculas na variável `texto` e armazenamos o resultado na variável `nova_string`. Em seguida, imprimimos o valor da nova string na tela. O resultado seria "ESSA É UMA STRING DE TESTE".

Também é possível capitalizar apenas a primeira letra de cada palavra usando o comando `sed` da seguinte forma:

```
# Usando o comando sed para capitalizar a primeira letra de cada palavra
nova_string=$(echo $texto | sed 's/\b\w/\u&/g')

# Imprimindo a nova string capitalizada
echo $nova_string
```

Neste caso, o resultado seria "Essa É Uma String De Teste".

## Aprofundando-se na capitalização de strings

Ambos os comandos `tr` e `sed` são ferramentas poderosas para manipulação de texto em Bash, e podem ser usados de diversas formas para capitalizar strings. Outras opções incluem o comando `awk` e usar expressões regulares com o comando `grep`.

Também é importante mencionar que a capitalização de strings pode variar de acordo com o idioma e a gramática utilizados, então é sempre importante entender as regras específicas para cada situação.

## Veja também

- Documentação do comando `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Documentação do comando `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Tutorial sobre expressões regulares em Bash: https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions