---
title:    "Fish Shell: Lendo um arquivo de texto"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto com o Fish Shell?

Se você é um programador(a) que trabalha com o Fish Shell, provavelmente já se deparou com a necessidade de ler um arquivo de texto em algum momento. Isso pode ser útil para obter informações de configuração, dados de entrada ou até mesmo para manipular arquivos de log. Neste artigo, vamos aprender como ler um arquivo de texto com o Fish Shell e explorar algumas possibilidades mais avançadas.

## Como ler um arquivo de texto com o Fish Shell

A leitura de um arquivo de texto com o Fish Shell é bastante simples e pode ser feita utilizando o comando ```read```. Veja um exemplo básico:

```Fish Shell
set arquivo (cat arquivo.txt)
```

O comando ```cat``` aqui vai imprimir todo o conteúdo do arquivo e o redirecionamento para a variável ```arquivo``` através do ```set``` permite que esse conteúdo seja armazenado para uso posterior. Agora, podemos acessar esse conteúdo através da variável ```arquivo``` usando o comando ```echo```:

```Fish Shell
echo $arquivo
```

Isso imprimirá o conteúdo do arquivo na tela. Mas e se quisermos ler apenas uma linha específica do arquivo? Podemos usar o comando ```head``` para isso:

```Fish Shell
set linha (head -n 1 arquivo.txt)
echo $linha
```

Neste exemplo, estamos lendo apenas a primeira linha do arquivo e armazenando na variável ```linha```, que é então impressa na tela. Existem também outros comandos úteis para ler arquivos de texto, como ```tail``` para ler as últimas linhas do arquivo e ```grep``` para procurar por padrões específicos dentro do conteúdo.

## Mergulhando mais fundo

Além dos comandos básicos, o Fish Shell também possui recursos mais avançados para lidar com a leitura de arquivos de texto. Podemos usar o comando ```while``` em conjunto com o ```read``` para percorrer todo o conteúdo de um arquivo linha por linha. Veja um exemplo:

```Fish Shell
while read linha
    echo $linha
end < arquivo.txt
```

Esse código imprimirá todas as linhas do arquivo na tela. Também é possível utilizar loops e estruturas condicionais para processar o conteúdo do arquivo de forma mais complexa.

## Veja também

- Documentação oficial do Fish Shell sobre o comando ```read```: https://fishshell.com/docs/current/commands.html#read
- Tutorial sobre a leitura de arquivos de texto com o Fish Shell: https://devhints.io/fish-shell
- Artigo sobre manipulação de arquivos com o Fish Shell: https://scotch.io/tutorials/how-to-use-fish-shell-to-manipulate-files