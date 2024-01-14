---
title:    "Bash: Escrevendo para o erro padrão"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que utilizar a escrita no erro padrão?

Quando se trata de programação, muitas vezes nos deparamos com bugs e erros que podem dificultar a execução correta do nosso código. Aqui entra a importância da escrita no erro padrão, uma técnica que permite a identificação e correção de erros de forma mais eficiente.

## Como fazer:

Para escrever em erro padrão em Bash, utilizamos o comando "2>", seguido pelo arquivo em que desejamos salvar as mensagens de erro. Por exemplo:

```Bash
ls arquivo_inexistente 2> erros.txt
```

Isso irá redirecionar qualquer mensagem de erro gerada pelo comando "ls" para o arquivo "erros.txt". Podemos então analisar esse arquivo para verificar os erros e corrigi-los.

## Mergulho Profundo:

Existem algumas coisas importantes a se ter em mente ao utilizar a escrita no erro padrão. Primeiro, é preciso entender que apenas as mensagens de erro são redirecionadas, as mensagens de sucesso ainda serão exibidas na saída padrão. Além disso, podemos utilizar o código de saída "2>&1" para redirecionar tanto as mensagens de erro quanto as de sucesso para o mesmo arquivo.

É importante também lembrar que a escrita no erro padrão só funciona para comandos, não para programas em linguagens como Python ou Java. Nesses casos, deve-se utilizar a saída de erro padrão diretamente no código do programa.

## Veja também:

- [Introdução ao Bash scripting](https://www.digitalocean.com/community/tutorials/how-to-write-a-simple-bash-script)
- [Guia completo sobre redirecionamento em Bash](https://www.linuxjournal.com/article/4403)
- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)