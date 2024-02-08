---
title:                "Convertendo uma data em uma string"
aliases:
- pt/c/converting-a-date-into-a-string.md
date:                  2024-02-03T17:54:01.727992-07:00
model:                 gpt-4-0125-preview
simple_title:         "Convertendo uma data em uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Quê?

Converter uma data em uma string em C envolve traduzir uma estrutura de data ou carimbo de tempo para um formato legível por humanos. Programadores frequentemente realizam essa tarefa para exibir datas em logs, interfaces de usuário ou quando armazenam datas em um formato baseado em texto como JSON ou CSV.

## Como Fazer:

A função `strftime` da biblioteca `<time.h>` é comumente usada para esse propósito. Ela permite que você formate a data e a hora de várias maneiras, especificando os especificadores de formato. Aqui está um exemplo rápido:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Converter a data e hora em string (por exemplo, "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Data e Hora Atuais: %s\n", dateStr);
    return 0;
}
```

Um exemplo de saída pode parecer com isso:

```
Data e Hora Atuais: Wed Jun 30 21:49:08 2021
```

Você pode personalizar o formato alterando os especificadores de formato passados para `strftime`. Por exemplo, para obter a data no formato `AAAA-MM-DD`, você usaria `"%Y-%m-%d"`.

## Aprofundamento

A função `strftime` e a biblioteca `<time.h>` fazem parte da Biblioteca Padrão C, que remonta ao padrão ANSI C original (C89/C90). Embora direta e suportada em muitas plataformas, essa abordagem pode parecer de baixo nível e onerosa em comparação com linguagens de programação modernas que oferecem bibliotecas de data e hora mais intuitivas.

Deve-se notar, embora as funções de tempo da biblioteca padrão C sejam amplamente suportadas e relativamente simples de usar, elas carecem de algumas funcionalidades mais complexas de manipulação de fuso horário e internacionalização encontradas em bibliotecas de linguagens mais novas ou bibliotecas C de terceiros, como Componentes Internacionais para Unicode (ICU).

No entanto, as capacidades de personalização da função `strftime` e seu amplo suporte de plataforma a tornam uma ferramenta confiável e útil para conversão de strings de data em C. Programadores vindos de linguagens com bibliotecas de datetime de alto nível podem precisar se ajustar à sua natureza de baixo nível, mas a encontrarão notavelmente poderosa e versátil para formatar datas e horas para uma variedade de aplicações.
