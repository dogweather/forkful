---
title:                "C#: Obtendo a data atual"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em C#

Se você é novo na programação em C#, pode se perguntar por que precisamos obter a data atual em nossos programas. Bem, a resposta é simples - a data atual é uma informação essencial em muitos programas, pois pode ser usada para registrar transações, ordem de eventos ou apenas para fins informativos. Portanto, é importante saber como obter a data atual em seus projetos de C#.

## Como fazer

Em C#, existem várias maneiras de obter a data atual, dependendo do que você está procurando. Aqui estão alguns exemplos de código que você pode usar em seus programas:

```C#
// Obtendo a data atual como uma string no formato "DD-MM-AAAA"
string dataAtual = DateTime.Now.ToString("dd-MM-yyyy");

// Obtendo a data atual como um objeto DateTime
DateTime dataAtual = DateTime.Now;

// Obtendo apenas o dia atual como um inteiro
int diaAtual = DateTime.Now.Day;
```

E aqui está o que você pode esperar como saída desse código:

```
28-07-2021 // para o primeiro exemplo
28/07/2021 10:30:55 // para o segundo exemplo
28 // para o terceiro exemplo
```

## Profundidade

Agora, vamos falar um pouco mais sobre como a data atual é obtida em C#. Ao usar o método `DateTime.Now`, o sistema operacional do computador é consultado para retornar a data e hora atuais. Isso significa que a data e a hora obtidas serão baseadas na configuração do sistema do usuário. Portanto, se você estiver em um país diferente, poderá ver a data e hora no seu próprio fuso horário.

Além disso, o objeto `DateTime` em C# também fornece muitos outros métodos úteis que podem ajudá-lo a manipular a data e a hora de várias maneiras. Por exemplo, você pode adicionar ou subtrair dias, meses ou anos à data atual usando os métodos `Add()` ou `Subtract()`. Você também pode formatar a data de várias maneiras, usando o método `ToString()` e passando um argumento com o formato desejado, conforme mostrado nos exemplos acima.

## Veja também

- [Documentação oficial do método DateTime.Now em C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [Tutorial sobre manipulação de datas e horas em C#](https://www.c-sharpcorner.com/UploadFile/mahesh/manipulatingdatetime09032005130008PM/manipulatingdatetime.aspx) (em inglês)
- [Vídeo tutorial sobre como obter a data atual em C#](https://www.youtube.com/watch?v=tIv6dRFmH6A&t=91s) (em português)