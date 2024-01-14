---
title:                "Bash: Convertendo uma data em uma string"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você pode precisar converter uma data em formato de string ao escrever um script em Bash. Isso pode ser necessário para lidar com diferentes entradas de usuários ou para integração com outros sistemas que exigem uma data em formato de string.

## Como Fazer

Para converter uma data em formato de string, podemos usar o comando `date` em conjunto com opções específicas de formatação. Por exemplo, se quisermos exibir a data atual na forma de "YYYY-MM-DD", podemos usar o seguinte código:

```
Bash
date +%Y-%m-%d
```

Isso irá retornar o seguinte output:

```
2021-09-23
```

Outra opção útil é a de incluir o dia da semana na string, que pode ser feito adicionando `%A` à opção de formatação. Por exemplo:

```
Bash
date +%A, %Y-%m-%d
```

Isso irá retornar o seguinte output:

```
Thursday, 2021-09-23
```

Há muitas outras opções de formatação disponíveis, como exibir o mês por extenso, exibir o horário, entre outros. Experimente diferentes opções e veja qual se encaixa melhor nas suas necessidades.

## Deep Dive

O comando `date` é uma importante ferramenta no Bash, pois permite o uso de diferentes opções de formatação para retornar a data e a hora atuais conforme desejado. Porém, é importante notar que a saída retornada pode variar de acordo com o sistema operacional e sua configuração de data e hora. Portanto, é importante ter isso em mente ao utilizar o comando em seus scripts.

Além disso, é possível também usar o comando `printf` para formatar a saída da data em formato de string. Por exemplo:

```
Bash
printf "Hoje é %(%A, %Y-%m-%d)T\n"
```

Isso irá retornar o seguinte output:

```
Hoje é Thursday, 2021-09-23
```

Esse comando é útil quando precisamos armazenar a data em uma variável para ser usada posteriormente em nosso script.

## Veja também

- [Documentação oficial do comando `date` no Bash](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Como converter uma data em formato de string no Bash](https://www.shellhacks.com/pt/date-shell-script-linux-format-howto/)
- [Tutorial em vídeo sobre o uso do comando `date` no Bash](https://www.youtube.com/watch?v=JXRfgRKrlE8)

O comando `date` é um recurso poderoso no Bash que pode ser usado em diversas situações. Combiná-lo com o `printf` pode trazer ainda mais possibilidades de formatação. Esperamos que este mini-tutorial tenha sido útil e que você possa aplicar esse conhecimento em seus projetos futuros.