---
title:                "C++: Comparando duas datas"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que
Ao trabalhar com datas em programas de computador, pode ser necessário comparar duas datas para determinar qual delas é maior ou se são iguais. Isso pode ser útil em diversas situações, como em sistemas de reservas ou controle de estoque.

## Como
Em C++, podemos comparar duas datas utilizando o operador de comparação "==" (igual) ou "!=" (diferente). Porém, para isso, é preciso que as datas sejam representadas em uma forma que o compilador possa entender, como por exemplo, utilizando o formato "dd/mm/aaaa" ou "mm/dd/aaaa". Vejamos um exemplo de código:

```C++
#include<iostream>
using namespace std;

int main()
{
	// Declarando e inicializando duas datas
	string data1 = "15/05/2021";
	string data2 = "20/05/2021";

	// Comparando as datas utilizando o operador !=
	if(data1 != data2)
	{
		cout << "As datas sao diferentes." << endl;
	}

	// Comparando as datas utilizando o operador ==
	if(data1 == data2)
	{
		cout << "As datas sao iguais." << endl;
	}

	return 0;
}
```

A saída deste código será:

```
As datas sao diferentes.
```

## Deep Dive
Ao utilizar o operador de comparação "==" com datas, é importante ter em mente que ele irá comparar os valores do tipo `string` em ordem alfabética. Por isso, se as datas forem representadas em formato "dd/mm/aaaa", é preciso garantir que o dia venha primeiro, seguido do mês e, por último, o ano. Caso contrário, a comparação pode não ser precisa. 

Além disso, é importante lembrar que essa comparação será feita apenas com base no texto das datas, e não em seu valor numérico. Ou seja, se tivermos duas datas representadas em texto como "05/2021" e "10/2020", a primeira será considerada maior por ter o valor "05" antes do valor "10" na ordem alfabética.

## Veja também
- [Documentação oficial da linguagem C++ sobre operadores de comparação](http://www.cplusplus.com/doc/oldtutorial/operators/)
- [Tutorial sobre manipulação de datas em C++](https://www.geeksforgeeks.org/date-manipulation-in-c-c-and-python/)
- [Artigo sobre a importância de comparar datas em sistemas de informática](https://blog.cobli.co/a-impotancia-de-comparar-datas-em-sistema/)