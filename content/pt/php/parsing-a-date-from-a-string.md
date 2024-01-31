---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:37:42.122604-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Em PHP, converter uma string para uma data é pegar um texto que representa uma data e hora e transformá-lo num objeto `DateTime`. Fazemos isso para poder manipular datas de forma mais precisa e flexível no nosso código.

## Como Fazer:

Comecemos com um exemplo básico. Vou usar a função `date_create_from_format` para converter uma string para uma data.

```PHP
<?php
$dataString = '25-03-2023';
$dataObjeto = date_create_from_format('d-m-Y', $dataString);
echo $dataObjeto->format('Y-m-d'); // Saída: 2023-03-25
?>
```

Se recebermos uma data em formato ISO 8601, podemos usar a função `date_create`:

```PHP
<?php
$dataIsoString = '2023-03-25T15:30:00';
$dataObjetoIso = date_create($dataIsoString);
echo $dataObjetoIso->format('Y-m-d H:i:s'); // Saída: 2023-03-25 15:30:00
?>
```

E se a coisa der errado? Vamos pegar os erros:

```PHP
<?php
$dataStringErrada = '2023-02-30';
$dataObjetoErrado = date_create_from_format('Y-m-d', $dataStringErrada);
$erros = date_get_last_errors();
if ($erros['warning_count'] > 0 || $erros['error_count'] > 0) {
    print_r($erros);
}
?>
```

## Aprofundando:

A capacidade de interpretar strings de datas em PHP evoluiu bastante. Desde o PHP 5.2.0, temos a classe `DateTime`, que melhorou muito o trabalho com datas.

Antes disso, estávamos limitados a funções como `strtotime()`, que, apesar de úteis, tinham as suas limitações e não forneciam a mesma flexibilidade ou recursos de internacionalização.

Existem outras formas de lidar com datas, como o objeto `IntlDateFormatter` da extensão `intl` para formatar e analisar datas de maneira localizada.

Na prática, quando tratamos de parsear datas, há muitas opções. Escolher a função depende do seu caso específico. Por exemplo:

- `strtotime()` é útil para strings de datas em inglês e operações simples.
- `DateTime::createFromFormat()` permite mais controle sobre o formato e é ideal quando você conhece o formato de entrada da data.
- `IntlDateFormatter::parse()` é bom para projetos que exigem localização.

Coisas que você deveria saber:

- O PHP assumirá o fuso horário configurado no servidor se você não especificar um.
- Erros de parsing podem occur se o formato da data não combinar com a string fornecida.
- Funções de data e hora são afetadas pelas configurações locais e regionais.

## Veja Também:

- Documentação oficial do PHP sobre `DateTime`: https://www.php.net/manual/pt_BR/class.datetime.php
- Função `strtotime()`: https://www.php.net/manual/pt_BR/function.strtotime.php
- Extensão `Intl` e classe `IntlDateFormatter`: https://www.php.net/manual/pt_BR/class.intldateformatter.php
- A função `date_get_last_errors()`: https://www.php.net/manual/pt_BR/function.date-get-last-errors.php
