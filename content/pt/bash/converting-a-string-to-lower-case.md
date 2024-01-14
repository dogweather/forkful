---
title:                "Bash: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que Converter uma String para Minúsculas em Bash?

Às vezes, precisamos trabalhar com strings em nosso código Bash e pode ser útil convertê-las para minúsculas. Isso pode ser útil para comparações de strings, formatação de entrada do usuário ou para garantir que o texto esteja padronizado.

## Como Fazer

Para converter uma string para minúsculas em Bash, podemos usar o comando `tr` em conjunto com as opções `-d` e `-s`.

```
#!/bin/bash

# Definir a string inicial
string="EXEMPLO DE STRING EM MAIÚSCULAS"

# Usar tr para converter para minúsculas e armazenar em uma nova variável
nova_string=$(echo "$string" | tr 'A-Z' 'a-z')

# Imprimir a nova string
echo "Nova string em minúsculas: $nova_string"

# Saída:
# Nova string em minúsculas: exemplo de string em maiúsculas
```

Nesse exemplo, primeiro definimos uma variável com a string em maiúsculas. Em seguida, usamos o comando `tr` para substituir todos os caracteres em maiúsculas por caracteres em minúsculas e armazenamos o resultado em uma nova variável. Por fim, imprimimos a nova string.

Também é possível usar o comando `sed` para converter uma string para minúsculas.

```
#!/bin/bash

# Definir a string inicial
string="EXEMPLO DE STRING EM MAIÚSCULAS"

# Armazenar a string em uma nova variável e usar sed para converter para minúsculas
nova_string=$(echo "$string" | sed 's/./\L&/g')

# Imprimir a nova string
echo "Nova string em minúsculas: $nova_string"

# Saída:
# Nova string em minúsculas: exemplo de string em maiúsculas
```

Nesse exemplo, primeiro armazenamos a string em uma nova variável e, em seguida, usamos o `sed` para substituir cada caractere pela respectiva letra minúscula. O resultado é armazenado novamente na mesma variável e impresso.

## Explorando Mais a Conversão de Strings para Minúsculas em Bash

Uma das principais diferenças entre o uso de `tr` e `sed` para essa tarefa é que o `tr` converte todos os caracteres em maiúsculas, enquanto o `sed` mantém os caracteres não alfanuméricos na mesma forma. Além disso, podemos usar o comando `awk` para converter apenas a primeira letra de cada palavra em maiúscula, mantendo o resto em minúsculas.

```
#!/bin/bash

# Definir a string inicial
string="EXEMPLO DE STRING EM MAIÚSCULAS PARA TESTAR O AWK"

# Usar awk para converter a primeira letra de cada palavra em maiúscula e armazenar em uma nova variável
nova_string=$(echo "$string" | awk '{for(i=1;i<=NF;i++)sub(".", substr(tolower($i),1,1), $i); print}')

# Imprimir a nova string
echo "Nova string em maiúsculas: $nova_string"

# Saída:
# Nova string em maiúsculas: Exemplo De String Em Maiúsculas Para Testar O Awk
```

Nesse exemplo, usamos o `awk` para percorrer cada palavra da string e, em seguida, substituímos a primeira letra por sua versão em minúsculas. O resultado é armazenado em uma nova variável e impresso.

## Veja Também

- [Manual do Bash](https://www.gnu.org/software/bash/manual/)
- [Documentação do tr](https://man7.org/linux/man-pages/man1/tr.1.html)
- [Documentação do sed](https://man7.org/linux/man-pages/man1/sed.1.html)
- [Documentação do awk](https://man7.org/linux/man-pages/man1/awk.1.html)