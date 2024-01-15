---
title:                "Verificando se um diretório existe."
html_title:           "Bash: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Quando trabalhamos com scripts Bash, é importante verificar se determinado diretório existe antes de executar comandos ou operações. Isso pode evitar erros e garantir que o nosso script funcione corretamente.

## Como Fazer

Para verificar se um diretório existe em um script Bash, podemos usar o comando `test` com a opção `-d` seguido do caminho do diretório que queremos checar. Este é um exemplo simples:

```Bash
# Verifica se o diretório "Documentos" existe em $HOME
if test -d "$HOME/Documentos"; then
  echo "O diretório existe!"
else
  echo "O diretório não existe!"
fi
```

Se o diretório existir, o script imprime "O diretório existe!", caso contrário, imprime "O diretório não existe!". Podemos também usar o comando `&&` para continuar a execução do script apenas se o diretório existir:

```Bash
# Verifica se o diretório "Fotos" existe em /home/usuario
test -d "/home/usuario/Fotos" && echo "O diretório existe!"
```

Neste caso, se o diretório existir, o script imprime a mensagem. Se não existir, nada é executado.

## Deep Dive

O comando `test` usado para verificar a existência de um diretório é o mesmo utilizado para verificar outros tipos de arquivos ou condições, como se um arquivo é executável ou se uma variável está definida. Além disso, podemos usar as opções `-e` e `-f` para verificar a existência de qualquer tipo de arquivo e se um arquivo é regular, respectivamente.

Além disso, é importante lembrar de sempre usar aspas em torno de caminhos de diretórios ou arquivos, especialmente se eles contiverem espaços ou caracteres especiais.

## Veja Também

- [Tutorial Bash no Linux](https://www.vivaolinux.com.br/artigo/Programando-em-Bash)
- [Documentação do comando `test`](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Artigo sobre testes em Bash](https://linuxize.com/post/bash-check-if-file-exists/)