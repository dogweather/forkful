---
title:                "Bash: Verificando se um diretório existe"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Ao escrever scripts em Bash, muitas vezes precisamos verificar se um diretório existe antes de executar determinadas ações. Isso pode ser útil para evitar erros e garantir que o script funcione corretamente.

## Como fazer

Verificar a existência de um diretório em Bash é simples e pode ser feito utilizando o comando `test` seguido da opção `-d` para indicar que queremos verificar a existência de um diretório e, em seguida, o caminho para o diretório que desejamos verificar. Por exemplo:

``` Bash
test -d /home/user/diretorio
```

O comando acima irá retornar um código de saída `0` se o diretório existir e um código `1` caso contrário. Para tornar o processo mais amigável, você pode adicionar um `if` statement para realizar as ações desejadas com base no resultado do comando. Veja um exemplo de como isso pode ser feito:

``` Bash
if test -d /home/user/diretorio; then
    echo "O diretório existe!"
else
    echo "O diretório não existe."
fi
```

Você também pode usar o operador de lógica `&&` para executar comandos somente se o diretório existir, por exemplo:

``` Bash
test -d /home/user/diretorio && echo "O diretório existe!"
```

Outra opção é usar o comando `mkdir` com a opção `-p` para criar o diretório se ele não existir, evitando assim a necessidade de verificar sua existência antes de cria-lo.

## Deep Dive

O comando `test` é na verdade um alias para o comando `[`, que é um programa externo. Isso significa que também podemos utilizar o comando `[` para verificar a existência de um diretório. Ambos os comandos funcionam da mesma maneira e possuem opções similares, portanto é uma questão de preferência pessoal qual utilizar.

É importante ressaltar que a verificação da existência de um diretório não é feita apenas para tornar o código mais elegante, mas também para garantir a segurança do programa. Por exemplo, se você estiver usando o comando `rm -rf $PATH`, sem verificar antes a existência do diretório, poderá apagar acidentalmente todos os arquivos do sistema.

## Veja também

- [Documentação do comando `test` (em inglês)](https://linux.die.net/man/1/test)
- [Documentação do comando `mkdir` (em inglês)](https://linux.die.net/man/1/mkdir)