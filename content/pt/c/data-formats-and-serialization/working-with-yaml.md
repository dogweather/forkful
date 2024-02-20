---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:32.341087-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML N\xE3o \xE9\
  \ uma Linguagem de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados\
  \ leg\xEDvel por humanos que pode\u2026"
lastmod: 2024-02-19 22:05:06.142708
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML N\xE3o \xE9 uma\
  \ Linguagem de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados\
  \ leg\xEDvel por humanos que pode\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O Que & Por Que?

YAML, que significa "YAML Ain't Markup Language" (YAML Não é uma Linguagem de Marcação), é um padrão de serialização de dados legível por humanos que pode ser utilizado para todo tipo de aplicação, desde arquivos de configuração até armazenamento de dados. Programadores frequentemente trabalham com YAML quando precisam de um formato fácil de ler e escrever para arquivos de configuração ou troca de dados entre linguagens e sistemas.

## Como fazer:

Trabalhar com YAML em C requer uma biblioteca, já que a biblioteca padrão do C não oferece suporte direto para análise ou serialização de YAML. Uma das bibliotecas YAML mais populares para C é a `libyaml`, que oferece interfaces de baixo e alto nível para análise e emissão de YAML. Abaixo está um exemplo de como analisar um arquivo YAML simples usando `libyaml`:

**Primeiro**, você precisa instalar a biblioteca `libyaml`. Se você estiver em um sistema do tipo Unix, geralmente pode instalá-la através do seu gerenciador de pacotes. Por exemplo, no Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Em seguida**, considere um arquivo YAML simples chamado `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Aqui está** um exemplo básico de como analisar este arquivo YAML em C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Falha ao inicializar o analisador de YAML!\n", stderr);

    if (fh == NULL)
        fputs("Falha ao abrir arquivo!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Valor: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Este programa simples abre um arquivo YAML, inicializa o analisador de YAML e lê o arquivo, imprimindo os valores escalares (neste exemplo, os campos do nosso YAML simples). Note que a verificação de erros é mínima neste exemplo simples e deve ser mais robusta em código de produção.

Executar o programa com nosso `config.yaml` produzirá:

```plaintext
Valor: John Doe
Valor: 29
Valor: false
```

## Aprofundamento

O YAML foi lançado pela primeira vez em 2001 e projetado para ser mais legível e amigável ao usuário do que outros formatos de serialização de dados como XML ou JSON, pegando emprestado de várias linguagens como C, Perl e Python para sua filosofia de design. Apesar de suas vantagens em legibilidade e facilidade de modificação humana, YAML pode ser complexo para ser analisado programaticamente devido à sua dependência de indentação e seu vasto conjunto de recursos, incluindo referências e tipos personalizados.

Embora `libyaml` forneça um acesso robusto de baixo nível para a análise e emissão de YAML em C, pode ser incômodo para tarefas simples devido à sua API verbosa. Por essas razões, alguns programadores preferem usar bibliotecas de mais alto nível ou até mesmo outros formatos de serialização de dados como JSON ao trabalhar em C, especialmente quando a análise performática com mínimo excesso de código é uma prioridade. No entanto, YAML continua sendo uma escolha popular para arquivos de configuração e situações onde a legibilidade humana é primordial. Alternativas como TinyYAML ou a incorporação de um interpretador de alto nível (por exemplo, incorporando Python ou Lua) poderiam fornecer mais conveniência para aplicações específicas, equilibrando entre a facilidade de uso e as necessidades de desempenho.
