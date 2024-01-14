---
title:    "Bash: Buscar e substituir texto"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que usar Bash para busca e substituição de texto?

A busca e substituição de texto é uma tarefa comum na programação, especialmente quando lidando com grandes quantidades de dados. No entanto, fazê-lo manualmente pode ser demorado e propenso a erros. O uso do Bash para realizar essa tarefa pode tornar o processo mais rápido e eficiente, economizando tempo e garantindo maior precisão.

## Como fazer busca e substituição de texto em Bash?

Para fazer busca e substituição de texto em Bash, você pode usar o comando `sed` (Stream Editor). Este comando permite que você procure por padrões de texto específicos e os substitua por outro texto. Aqui está um exemplo de como usá-lo:

```
$ cat exemplo.txt

Este é um exemplo de texto para ser modificado.

$ sed 's/exemplo/teste/' exemplo.txt

Este é um teste de texto para ser modificado.
```

Neste exemplo, usamos o comando `cat` para mostrar o conteúdo do arquivo `exemplo.txt`, que contém a frase "Este é um exemplo de texto para ser modificado.". Em seguida, usamos o `sed` para procurar a palavra "exemplo" e substituí-la por "teste", resultando em "Este é um teste de texto para ser modificado." É importante notar que essa substituição é feita somente no output, não alterando o arquivo original.

Você também pode usar o `sed` para fazer substituições em um arquivo específico, em vez de apenas no output:

```
$ sed -i 's/exemplo/teste/' exemplo.txt
```

O parâmetro `-i` (ou `--in-place`) indica que a substituição deve ser feita no arquivo original.

## Aprofundando na busca e substituição de texto em Bash

Além de substituir diretamente o texto, o `sed` pode ser usado para fazer outras tarefas, como excluir linhas ou adicionar novas linhas de texto. Ele também suporta a utilização de expressões regulares, o que permite maior flexibilidade e precisão nas substituições. É possível também usar variáveis e loops em conjunto com o `sed` para realizar substituições em massa em vários arquivos.

Outra ferramenta útil para busca e substituição de texto em Bash é o comando `grep`, que permite que você procure por padrões de texto e exiba as linhas correspondentes. Por exemplo:

```
$ grep "exemplo" exemplo.txt

Este é um exemplo de texto para ser modificado.
```

Este comando exibirá apenas a linha que contém o padrão de texto "exemplo". Você também pode usar o `grep` em conjunto com o `sed` para fazer substituições somente nas linhas em que o padrão de texto for encontrado.

## Veja também

- [Tutorial de busca e substituição de texto em Bash](https://www.lifewire.com/sed-command-2618008)
- [Documentação do comando `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial básico de expressões regulares em Bash](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)
- [Outros comandos úteis para manipulação de texto em Bash](https://www.cyberciti.biz/faq/understanding-bash-shell-config-file/)