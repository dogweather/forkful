---
title:                "Obtendo a data atual"
html_title:           "C++: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Há muitos casos em que você precisará saber a data atual em um programa C++. Pode ser para registrar quando um evento ocorreu, para fins de rastreamento ou para gerar relatórios com informações atualizadas.

## Como fazer

Para obter a data atual em um programa C++, você pode usar a função `std::time` da biblioteca `chrono`. Aqui está um exemplo de código que imprime a data atual:

```C++
#include <iostream>
#include <chrono>

int main()
{
	// Obtendo o tempo atual
	auto tempo_atual = std::chrono::system_clock::now();
	// Convertendo para o formato de data
	auto data_atual = std::chrono::system_clock::to_time_t(tempo_atual);
	// Imprimindo data atual
	std::cout << "Data atual: " << std::ctime(&data_atual) << std::endl;
}
```

A saída deste código será algo como:

```
Data atual: Sat Sep 26 13:57:06 2020
```

Você também pode formatar a data de acordo com suas necessidades usando a função `std::put_time`. Este exemplo mostra como imprimir a data no formato "DD/MM/AAAA":

```C++
#include <iostream>
#include <iomanip>
#include <chrono>

int main()
{
    // Obtendo o tempo atual
	auto tempo_atual = std::chrono::system_clock::now();
	// Convertendo para o formato de data
	auto data_atual = std::chrono::system_clock::to_time_t(tempo_atual);
	// Formatando data
	std::cout << "Data atual: " << std::put_time(std::localtime(&data_atual), "%d/%m/%Y") << std::endl;
}
```

A saída será:

```
Data atual: 26/09/2020
```

## Mergulho Profundo

A função `std::time` retorna o tempo atual como um objeto do tipo `std::chrono::time_point`. Este é um objeto que representa uma duração de tempo em relação a um determinado ponto no tempo. Você também pode usar outras funções como `std::gmtime` e `std::localtime` para converter o tempo em outras representações, como datas no formato UTC ou hora local.

## Veja também

- [Documentação da biblioteca Chrono do C++](https://en.cppreference.com/w/cpp/header/chrono)
- [Tutorial de data e hora em C++](https://www.cplusplus.com/reference/ctime/)
- [Manipulação de hora e data em C++](https://www.geeksforgeeks.org/time-manipulation-in-c-with-chrono-header/)