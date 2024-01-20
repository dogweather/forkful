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

## O Que & Porquê?

Verificar se uma diretoria existe é identificar se um caminho especificado é uma pasta no sistema de arquivos. Programadores fazem isso para evitar erros como tentar ler ou escrever em uma pasta que não existe.

## Como Fazer:

Para verificar se uma diretoria existe, você pode usar o comando `[ -d ]`:

```Bash
if [ -d "/caminho/para/a/diretoria" ]; then
  echo "A diretoria existe."
else
  echo "A diretoria não existe."
fi
```

Exemplo de saída se a diretoria existir:

```
A diretoria existe.
```

Exemplo de saída se a diretoria não existir:

```
A diretoria não existe.
```

Você também pode usar `[[ -d ]]` para a mesma verificação, o qual é mais moderno e suporta características adicionais como operadores lógicos:

```Bash
[[ -d "/caminho/para/a/diretoria" ]] && echo "Existe" || echo "Não existe"
```

## Aprofundamento:

Historicamente, verificação de existência de diretórios é uma necessidade básica nos scripts Bash para prevenir a execução de comandos em diretórios errados, o que poderia levar a resultados inesperados ou danos ao sistema. Alternativamente, além do teste `-d`, existe o comando `test` que pode ser usado para o mesmo propósito, embora em forma menos legível. Os detalhes de implementação no Bash envolvem a verificação das informações do inode do sistema de arquivos para determinar o tipo do caminho especificado.

Antes do Bash, em shells mais antigos, os programadores muitas vezes tinham que criar diretórios sem a verificação prévia, resultando em mensagens de erro ou diretórios duplicados. O Bash trouxe comandos de teste integrados, como `[` e `[[`, melhorando a escrita de scripts seguros e eficientes.

Quando se trata de alternativas, o uso de `-e` ou `-f` pode verificar a existência de arquivos e diretórios ou apenas arquivos, respectivamente.

## Ver Também:

- [Bash Conditional Expressions](https://www.gnu.org/software/bash/manual/bash.html#Bash-Conditional-Expressions)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)
- [Stack Overflow: How to check if a directory exists in a Bash shell script?](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script)