---
title:                "Verificando se um diretório existe"
html_title:           "Bash: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Verificar se um diretório existe é simplesmente verificar se um certo caminho de diretório existe no sistema de arquivos. Os programadores geralmente fazem isso para garantir que seu código funcione corretamente e não cause erros.

## Como fazer:

Para verificar se um diretório existe, podemos usar o comando ```[ -d <diretório> ] ```. Isso irá retornar 0 se o diretório existir e 1 se não existir. Podemos utilizar essa condição em um `if` statement para executar ações diferentes dependendo do resultado.

Exemplo de código:

```
if [ -d diretório_existente ]; then
  echo "O diretório existe!"
else
  echo "O diretório não existe."
fi
```

Saída: O diretório existe!

Também podemos usar o comando `test` para verificar se um diretório existe:

```
if test -d diretório_existente; then
  echo "O diretório existe!"
else
  echo "O diretório não existe."
fi
```

Saída: O diretório existe!

## Deep Dive:

Verificar se um diretório existe é uma tarefa comumente realizada por programadores em diversos contextos, como por exemplo, em scripts de automação de tarefas ou em programas que manipulam arquivos e pastas. Essa funcionalidade está disponível em diversas linguagens de programação, mas o Bash torna isso muito simples com o uso do comando `test` ou do comando `[`.

Além disso, também podemos usar o comando `ls` para verificar se um diretório existe, mas ele também exibirá o conteúdo do diretório, o que pode não ser ideal em algumas situações.

## Veja também:

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Artigo da Linuxize sobre verificar se um arquivo ou diretório existe](https://linuxize.com/post/bash-check-if-file-exists/)
- [Vídeo do canal Linux Hint no YouTube sobre verificar se um diretório existe](https://www.youtube.com/watch?v=XLORXoSYJ8c)