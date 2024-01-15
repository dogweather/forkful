---
title:                "Calculando uma data no futuro ou passado"
html_title:           "C: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular uma data no futuro ou no passado pode ser uma tarefa útil em várias aplicações, como sistemas de reservas, agendas e outras ferramentas de gerenciamento do tempo. Além disso, é uma habilidade importante para programadores que desejam criar aplicações mais completas e funcionais.

## Como fazer

Para calcular uma data no futuro ou no passado em linguagem C, precisamos manipular as informações de data e hora. Uma maneira de fazer isso é utilizando a struct "tm" da biblioteca padrão <time.h>. Veja um exemplo de código abaixo:

```C
#include <stdio.h>
#include <time.h>

int main(){
	//Criando uma struct tm com a data atual
	struct tm data_atual;
	time_t tempo_atual;
	tempo_atual = time(NULL);
	data_atual = *localtime(&tempo_atual);

	//Adicionando 10 dias à data atual
	data_atual.tm_mday += 10;
	mktime(&data_atual);

	//Imprimindo a nova data
	printf("Data daqui a 10 dias: %d/%d/%d", data_atual.tm_mday, data_atual.tm_mon+1, data_atual.tm_year+1900);
	return 0;
}
```

A saída desse código seria "Data daqui a 10 dias: 20/9/2020", considerando que o código foi executado em 10 de setembro de 2020. Como podemos ver, primeiro criamos uma struct "tm" com a data atual e utilizamos a função localtime() para preenchê-la. Em seguida, utilizamos a função mktime() para corrigir a data e hora da struct, levando em conta possíveis alterações que fizemos. Por fim, basta acessar os membros da struct "tm" para obter as informações desejadas.

## Mergulho Profundo

Além de adicionar uma quantidade específica de tempo, como fizemos no exemplo acima, também é possível utilizar outras funções para manipular datas. Por exemplo, a função difftime() pode ser utilizada para calcular a diferença em segundos entre duas datas. Já a função strftime() pode ser utilizada para formatar uma data de acordo com um padrão especificado.

É importante lembrar que a struct "tm" possui mais membros do que os mostrados no exemplo, como informações sobre o fuso horário e horário de verão. Portanto, é importante consultar a documentação para utilizar corretamente todas as funcionalidades dessa biblioteca.

## Veja também

- [Documentação oficial da struct "tm"](https://www.cplusplus.com/reference/ctime/tm/)
- [Tutorial sobre manipulação de datas em C](https://www.daniweb.com/programming/software-development/threads/476004/manipulating-date-and-time-in-c)