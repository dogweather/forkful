---
title:    "Fish Shell: Comparando duas datas"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que comparar duas datas utilizando o Fish Shell?
Comparar datas é uma tarefa comum na programação, especialmente quando se trabalha com dados relacionados a tempo. No Fish Shell, há uma maneira fácil e eficiente de comparar duas datas, o que pode economizar seu tempo e tornar seu código mais eficiente.

## Como fazer isso:
Para comparar duas datas utilizando o Fish Shell, você precisará utilizar a função `date` e o operador de comparação `-gt` para verificar se uma data é maior que a outra. Veja um exemplo abaixo:

```Fish Shell
if [ (date -d "2020-09-01" +%s) -gt (date -d "2020-08-01" +%s) ]
  echo "A primeira data é posterior à segunda data"
end
```

Neste exemplo, utilizamos a função `date` para converter as datas para o formato de timestamp, que representa o número de segundos desde a época UNIX (1 de janeiro de 1970). O operador `-gt` então compara os dois timestamps e retorna verdadeiro ou falso.

## Uma análise detalhada:
Para entender melhor como esse código funciona, é importante entender como o Fish Shell lida com datas. A função `date` utiliza o formato de data ISO 8601, que é amplamente utilizado e reconhecido como um padrão universal. Este formato é representado da seguinte maneira: `AAAA-MM-DD`, onde `AAAA` representa o ano, `MM` representa o mês e `DD` representa o dia.

Ao utilizar a opção `-d` seguida de uma data, a função `date` irá convertê-la para o formato de timestamp, que é um número inteiro. Isso permite que as datas sejam facilmente comparadas utilizando o operador `-gt`.

Além disso, é importante notar que, como a função `date` é baseada no sistema `date` do Linux, os usuários do Fish Shell precisam ter o GNU Core Utilities instalado em seus sistemas para utilizar esta função.

## Veja também
- Guia para iniciantes do Fish Shell (em Português): https://fishshell.com/docs/current/tutorial.html
- Documentação oficial do Fish Shell (em Inglês): https://fishshell.com/docs/current/index.html
- Página do projeto GNU Core Utilities (em Inglês): https://www.gnu.org/software/coreutils/