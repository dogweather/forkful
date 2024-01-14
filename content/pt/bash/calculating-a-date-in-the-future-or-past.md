---
title:    "Bash: Calculando uma data no futuro ou no passado"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que

O cálculo de datas no futuro ou passado pode ser necessário em certos projetos de programação. Essa habilidade permite que o programador forneça informações precisas sobre eventos futuros ou passados.

## Como Fazer

Para calcular datas no futuro ou passado, podemos usar o comando `date` no Bash. O formato básico é o seguinte: `date -d 'quantity time-unit'.`

Onde `quantity` é um número e `time-unit` pode ser qualquer unidade de tempo, como anos, meses, dias, horas, minutos, segundos, entre outros. Por exemplo, para calcular a data daqui a 2 semanas, podemos usar o seguinte comando: 
```Bash
date -d '2 weeks'
```
Isso nos dará a data exata de dois semanas a partir do dia atual.

Também podemos especificar uma data específica como ponto de partida. Por exemplo, se quisermos saber qual será a data daqui a 3 meses e 2 dias a partir de 1º de janeiro de 2022, podemos usar o seguinte comando:
```Bash
date -d '2022-01-01 + 3 months + 2 days'
```
Isso nos dará a data exata de 3 meses e 2 dias após 1º de janeiro de 2022.

## Deep Dive

Além dos exemplos acima, existem várias opções e parâmetros adicionais que podem ser usados no comando `date` para calcular datas no futuro ou passado. Por exemplo, podemos usar a opção `-r` para especificar um arquivo como ponto de partida. O comando `date` então usará a data de modificação desse arquivo como referência para o cálculo do tempo desejado.

Também podemos usar a opção `-v` para adicionar ou subtrair um valor de tempo de uma data existente. Isso pode ser útil quando quisermos calcular uma data com base em uma data anterior, como "3 meses após o meu aniversário" ou "2 semanas antes da data de entrega do projeto".

Para ver todas as opções e detalhes de formatação disponíveis, podemos usar o comando `man date` no terminal para acessar o manual do comando `date`.

## Veja também

- [Documentação oficial do comando `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Tutorial de cálculo de datas no Bash](https://www.baeldung.com/linux/add-days-date-command)
- [Artigo sobre formatação de datas no Bash](https://www.howtogeek.com/531949/how-to-format-date-and-time-in-the-linux-shell-part-1/)