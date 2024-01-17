---
title:                "Verificando se um diretório existe"
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que e Porque?
Verificar se um diretório existe é um processo importante para os programadores. Isso envolve verificar se um determinado caminho no sistema de arquivos é um diretório válido ou não. A razão pela qual os programadores fazem isso é para garantir que seu código possa lidar adequadamente com diferentes situações e evitar erros no programa.

## Como fazer:
Existem várias maneiras de verificar se um diretório existe em Fish Shell. Uma maneira simples é usar o comando `test` seguido do operador `-d` para verificar o tipo de arquivo. Por exemplo, para verificar se o diretório "documents" existe, podemos usar o seguinte comando:

```
test -d documents
```

Isso retornará um código de saída 0 se o diretório existir e um código de saída 1 se não existir.

Outra opção é usar o comando `stat` para obter informações sobre um arquivo ou diretório específico. Por exemplo, para verificar se o diretório "pictures" existe, podemos usar o seguinte comando:

```
if stat pictures
  echo "O diretório existe!"
else
  echo "O diretório não existe."
end
```

Este comando irá imprimir "O diretório existe!" se o diretório "pictures" existe, e "O diretório não existe." se não existir.

## Mergulho Profundo:
Verificar se um diretório existe não é uma tarefa nova para os programadores. De fato, é uma tarefa comum em muitas linguagens de programação e sistemas operacionais. Além dos métodos acima mencionados, também é possível usar o comando `ls` ou a função `os.path.isdir()` em Python para verificar se um diretório existe.

No entanto, é importante mencionar que verificar se um diretório existe não é a mesma coisa que acessá-lo. Um diretório pode existir no sistema de arquivos, mas pode não ter permissão de acesso. Portanto, é importante considerar também a verificação de permissões ao trabalhar com diretórios.

## Veja também:
- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Guia rápido de Fish Shell](https://fishshell.com/docs/current/#quick-start)
- [Como verificar se um diretório existe em outras linguagens de programação](https://www.codecademy.com/articles/how-to-check-if-directory-exists-python)