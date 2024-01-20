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

## O Que & Por Que?
Checar se um diretório existe é verificar se há uma pasta específica num sistema de arquivos. Programadores fazem isso para prevenir erros antes que tentem acessar, modificar ou remover diretórios que podem não estar presentes.

## Como fazer:
Aqui estão alguns exemplos de como fazer isso no Bash. Em cada exemplo, assumimos que o diretório que queremos verificar é `/tmp/testdir`.

Para verificar se o diretório existe:

```Bash
if [ -d "/tmp/testdir" ] 
then
    echo "O diretório existe."
else
    echo "O diretório não existe."
fi
```
Output esperado quando o diretório existir: `O diretório existe.`
Output esperado quando o diretório não existir: `O diretório não existe.`

## Deep Dive

Bash é uma interface de linha de comando do usuário que foi lançada pela primeira vez em 1989. A verificação da existência de um diretório é uma operação comum e antiga em programação que permite que o desenvolvedor manuseie erros de I/O eficientemente.

Na verdade, o comando '-d' não é a única maneira de verificar a existência de um diretório em Bash. '-e' também pode ser usado para verificar se qualquer tipo de arquivo (incluindo diretórios) existe.

```Bash
if [ -e "/tmp/testdir" ] 
then
    echo "O arquivo ou diretório existe."
else
    echo "O arquivo ou diretório não existe."
fi
```

Claro, em termos de detalhes de implementação, é importante notar que esses comandos Bash funcionarão apenas em sistemas operacionais baseados em UNIX, como Linux ou macOS.

## Veja Também

Para mais detalhes ou para resolver qualquer dúvida, você pode verificar os links abaixo:

1. Manual do Bash: https://www.gnu.org/software/bash/manual/bash.html
2. Referência do Bash: https://tldp.org/LDP/abs/html/testconstructs.html#DBLBRACKETS 
3. Perguntas frequentes sobre o Bash de Greg's Wiki: https://mywiki.wooledge.org/BashFAQ/031