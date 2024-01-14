---
title:    "C++: Convertendo uma data em uma string"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Porque
Você provavelmente já se viu em uma situação em que precisava converter uma data em C++ em uma string legível para o usuário. Essa é uma tarefa comum em muitos projetos, e entender como realizar essa conversão pode facilitar muito o seu trabalho.

## Como fazer
Converter uma data em uma string em C++ pode parecer intimidante à primeira vista, mas com os recursos certos, é bastante simples. Primeiro, vamos entender os tipos de dados que serão utilizados: `time_t` e `tm`. O `time_t` representa o tempo como o número de segundos desde 1º de janeiro de 1970, enquanto o `tm` é uma estrutura que armazena os componentes da data e hora, como dia, mês, ano e hora. Com esses tipos em mente, podemos usar a função `strftime()` para formatar nossa data em uma string personalizada. Veja um exemplo abaixo:

```
#include <iostream>
#include <ctime>

int main() {
  // Obtemos o tempo atual
  time_t agora = time(nullptr);

  // Convertendo o tempo atual para uma estrutura tm
  tm *data_atual = localtime(&agora);

  // Cria uma string formatada com o dia, mês, ano e hora
  char string_data[30];
  strftime(string_data, 30, "%d/%m/%Y %H:%M", data_atual);

  // Exibe a string formatada
  std::cout << "Data atual: " << string_data << std::endl;

  return 0;
}
```

A saída desse código será algo como `Data atual: 19/09/2020 15:25`. Perceba que a função `strftime()` recebe três argumentos: uma string para armazenar o resultado, o tamanho dessa string e um ponteiro para a estrutura `tm` que contém os componentes da data e hora. O terceiro argumento é opcional e permite que você personalize o formato da string.

## Deep Dive
Além do formato `%d/%m/%Y %H:%M` utilizado no exemplo acima, a função `strftime()` suporta uma ampla variedade de formatos para personalizar a sua string. Alguns exemplos incluem:

- `%d`: Dia do mês com dois dígitos (01 a 31)
- `%m`: Mês com dois dígitos (01 a 12)
- `%Y`: Ano com quatro dígitos
- `%H`: Hora com dois dígitos (00 a 23)
- `%M`: Minutos com dois dígitos (00 a 59)
- `%S`: Segundos com dois dígitos (00 a 59)
- `%b`: Nome abreviado do mês (Jan, Fev, Mar, etc.)
- `%a`: Nome abreviado do dia da semana (Seg, Ter, Qua, etc.)

Há muitas outras opções disponíveis, e você pode escolher as que melhor se adequam às suas necessidades.

## Veja também
- [Documentação oficial da função `strftime()`](https://devdocs.io/cpp/function/strftime)
- [Mais informações sobre manipulação de datas em C++](https://www.geeksforgeeks.org/c-programming-for-dates-and-time/)

Com essas informações, esperamos que você se sinta mais confortável em converter datas em strings em seus projetos em C++. Lembre-se de consultar sempre a documentação oficial e de experimentar diferentes formatos para encontrar o que melhor se encaixa em suas necessidades. Boa codificação!