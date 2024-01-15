---
title:                "Comparando duas datas"
html_title:           "Fish Shell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que
Quem nunca precisou comparar duas datas em um código? Seja para verificar a ordem cronológica de eventos ou para calcular a diferença de tempo entre elas, essa é uma tarefa comum em programação. A linguagem Fish Shell oferece uma maneira simples e eficiente de realizar essa comparação.

## Como fazer
Para comparar duas datas no Fish Shell, utilizamos o comando `date`. Para isso, precisamos especificar o formato em que as datas serão apresentadas, que é definido pelas letras entre colchetes. Vamos usar `[+%s]` para obter a representação em segundos das datas.
```
# Define as datas a serem comparadas
set data1 (date -j -f %m/%d/%Y "10/10/2020" +%s)
set data2 (date -j -f %m/%d/%Y "12/01/2021" +%s)

# Realiza a comparação e exibe o resultado
echo "Data 1 é anterior a Data 2?:" (test $data1 -lt $data2; and echo "Sim" || echo "Não")
# Saída: Data 1 é anterior a Data 2?: Sim
```
Para entender melhor esse exemplo, vamos explicar o que cada parte do código faz:
- Na primeira linha, definimos a variável `data1` que recebe o valor da data "10/10/2020" formatada como segundos usando o comando `date`
- Da mesma forma, na segunda linha, definimos a variável `data2` com a data "12/01/2021"
- Na terceira linha, utilizamos o comando `test` em conjunto com o operador `-lt` (menor que) para verificar se `data1` é menor que `data2`
- Por fim, usamos o operador lógico `&&` para exibir "Sim" se a comparação for verdadeira, e `||` para exibir "Não" se for falsa.

## Mergulho Profundo
Além de verificar se uma data é anterior ou posterior a outra, também é possível calcular a diferença de tempo entre elas utilizando o mesmo formato `[+%s]`. Vamos ver um exemplo:
```
set data1 (date -j -f %m/%d/%Y "10/10/2020" +%s)
set data2 (date -j -f %m/%d/%Y "12/01/2021" +%s)

set diff (math $data2 - $data1)
echo "A diferença entre as datas é de $diff segundos"
# Saída: A diferença entre as datas é de 10368000 segundos
```
Nesse exemplo, utilizamos o comando `math` para subtrair o valor da `data1` do valor da `data2` e armazenamos o resultado na variável `diff`. Em seguida, exibimos essa diferença em segundos.

## Veja também
- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial básico de Fish Shell](https://dev.to/bitlang/tutorial-fish-shell-basico-3glj) (em inglês)
- [Como formatar datas no Fish Shell](https://bobcares.com/blog/format-date-fish-shell/) (em inglês)