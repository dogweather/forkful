---
title:    "Bash: Convertendo uma data em uma cadeia de caracteres"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que

Converter uma data em uma string é uma tarefa comum na programação Bash. Isso pode ser útil em situações como criar arquivos com um nome baseado na data atual ou exibir a data formatada em um script.

## Como fazer

Para converter uma data em uma string no Bash, usamos o comando `date`. Ele tem diversas opções para personalizar o formato da data e hora. Vamos dar uma olhada em alguns exemplos:

```Bash
# Exemplo 1: Data e hora atual no formato ISO 8601
$ date +%FT%T%z
2021-08-03T11:30:00-0300

# Exemplo 2: Data atual por extenso
$ date +"%A, %d de %B de %Y"
terça-feira, 03 de agosto de 2021
```

No primeiro exemplo, usamos os caracteres `%F`, `%T` e `%z` para formatar a data e hora de acordo com o padrão ISO 8601. No segundo exemplo, usamos os caracteres `%A`, `%d`, `%B` e `%Y` para exibir a data por extenso. Existem muitas outras opções que podem ser utilizadas, incluindo formatos personalizados.

## Mergulhando mais a fundo

O comando `date` usa o tempo do sistema para gerar a data e hora atual. No entanto, também é possível especificar uma data e hora específica utilizando a opção `-d` seguida de uma string com o formato desejado. Além disso, é recomendável sempre checar a documentação do `date` para se familiarizar com todas as opções disponíveis.

## Veja também

- Documentação do comando `date` no Linux: https://linux.die.net/man/1/date
- Tutorial sobre formatação de datas no Bash: https://www.lifewire.com/bash-date-command-3862192
- Guia de uso avançado do `date`: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/