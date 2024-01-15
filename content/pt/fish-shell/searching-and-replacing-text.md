---
title:                "Buscando e substituindo texto"
html_title:           "Fish Shell: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Imagine que você está editando um arquivo de código longo e precisa mudar o nome de uma variável em todo o documento. Em vez de passar manualmente por cada linha, a busca e substituição automatizada pode economizar tempo e evitar erros.

## Como fazer

Usando o Fish Shell, você pode facilmente realizar busca e substituição de texto usando o comando `sed`. Aqui está um exemplo:

```
Fish Shell - Busca e substituição de texto em um arquivo

# Criar um arquivo com texto de amostra
echo "Olá, mundo!" > texto.txt

# Usar sed para substituir "mundo" por "universo" em texto.txt
sed -i 's/mundo/universo/' texto.txt

# Verificar se a substituição foi feita corretamente
cat texto.txt

# Saída: "Olá, universo!"
```

## Explorando mais a fundo

O comando `sed` usado no exemplo é uma ferramenta poderosa para substituir texto em um arquivo. Ele usa expressões regulares para encontrar e substituir padrões específicos. Além disso, você também pode usar a opção `-i` para fazer a substituição diretamente no arquivo, em vez de imprimir a saída na tela.

Outra opção é usar o comando `grep` para procurar por um padrão específico em um arquivo e o `sed` para substituí-lo. Por exemplo:

```
# Usar grep para encontrar linhas que contenham a palavra "mundo"
grep -n "mundo" texto.txt

# Saída: 1:Olá, mundo!

# Usar sed para substituir "mundo" por "universo" apenas na linha encontrada
sed -i '1s/mundo/universo/' texto.txt

# Verificar se a substituição foi feita corretamente
cat texto.txt

# Saída: "Olá, universo!"
```

## Veja também

- [Fish Shell documentação oficial](https://fishshell.com/docs/current/index.html)
- [Guia de expressões regulares do sed](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)
- [Guia de comandos do grep](https://www.gnu.org/software/grep/manual/grep.html)