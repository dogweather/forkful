---
title:                "Obtendo a data atual."
html_title:           "Go: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que e por que?

Obter a data atual é um processo importante para programadores em Go. Isso permite que eles tenham acesso à data e hora exatas no momento da execução do programa. Isso é útil para registrar eventos, calcular tempos de execução e outras tarefas que dependem de informações temporais precisas.

## Como fazer:

Para obter a data atual em Go, você pode usar a função "Now()" do pacote "time". Ela retorna um objeto do tipo "Time" contendo a data e hora atuais.

```
Go time.Now()
```

Para exibir a data e hora em um formato legível, você pode usar o método "Format()", que aceita uma string de formatação como argumento. Por exemplo, para exibir a data em formato "dia/mês/ano":

```
Go time.Now().Format("02/01/2006")
```

O resultado será exibido como: "25/12/2021".

## Uma olhada mais profunda:

Em versões anteriores do Go, a função "Now()" não era tão eficiente e precisava de operações matemáticas adicionais para retornar a data e hora atual. Porém, na versão atual do Go, essa função usa o relógio do sistema operacional diretamente, tornando-a muito mais rápida e precisa.

Uma alternativa para obter a data atual em Go seria utilizar a função "Unix()" do pacote "time", que retorna a data e hora em formato Unix. Isso pode ser útil se o objetivo é comparar as datas ou armazená-las em um banco de dados.

## Veja também:

Para mais informações sobre a função "Now()" e suas opções de formatação, acesse a documentação oficial do pacote "time": https://golang.org/pkg/time/#Now.

Outra opção é usar a biblioteca "github.com/jinzhu/now", que possui funcionalidades adicionais para trabalhar com datas e horas em Go: https://github.com/jinzhu/now.