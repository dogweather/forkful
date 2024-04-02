---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:29.541317-07:00
description: "Obter a data atual em programa\xE7\xE3o \xE9 uma tarefa fundamental\
  \ que permite recuperar e manipular os dados de data e hora do sistema. Em tarefas\
  \ de scripting e\u2026"
lastmod: '2024-03-13T22:44:47.016397-06:00'
model: gpt-4-0125-preview
summary: "Obter a data atual em programa\xE7\xE3o \xE9 uma tarefa fundamental que\
  \ permite recuperar e manipular os dados de data e hora do sistema. Em tarefas de\
  \ scripting e\u2026"
title: Obtendo a data atual
weight: 29
---

## O Que & Por Que?
Obter a data atual em programação é uma tarefa fundamental que permite recuperar e manipular os dados de data e hora do sistema. Em tarefas de scripting e automação, é essencial para gerar timestamps, agendar tarefas e criar logs.

## Como fazer:
O Fish Shell utiliza comandos externos como `date` para obter a data atual, oferecendo flexibilidade para formatar a saída conforme necessário. Veja como usá-lo:

```fish
# Exibir a data atual no formato padrão
echo (date)

# Exemplo de saída: Qua 25 Out 2023 15:42:03 BST
```

Para customizar o formato da data, você pode usar a opção `+` seguida pelos especificadores de formato:

```fish
# Exibir a data atual no formato AAAA-MM-DD
echo (date "+%Y-%m-%d")

# Exemplo de saída: 2023-10-25
```

Para tarefas mais complexas, como trabalhar com timestamps ou realizar aritmética de datas, o Fish Shell depende de ferramentas externas como `date` devido à sua natureza de scripting. Aqui está um exemplo de como obter o atual timestamp UNIX:

```fish
# Obter o atual timestamp UNIX
echo (date "+%s")

# Exemplo de saída: 1666710123
```

E para adicionar um dia à data atual usando `date`:

```fish
# Adicionar um dia à data atual
echo (date -d "+1 day" "+%Y-%m-%d")

# Exemplo de saída: 2023-10-26
```

Nota: Os exemplos usam opções do comando `date` que funcionam com os GNU coreutils. As opções podem variar em outros ambientes como o macOS, que usa o comando BSD date por padrão. Sempre consulte `date --help` ou a página do manual para detalhes específicos do seu ambiente.
