---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:47.385974-07:00
description: "Recuperar a data atual no Bash envolve o uso de comandos internos para\
  \ exibir a data e a hora em v\xE1rios formatos. Programadores utilizam essa\u2026"
lastmod: '2024-02-25T18:49:44.382566-07:00'
model: gpt-4-0125-preview
summary: "Recuperar a data atual no Bash envolve o uso de comandos internos para exibir\
  \ a data e a hora em v\xE1rios formatos. Programadores utilizam essa\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O Que & Por Que?
Recuperar a data atual no Bash envolve o uso de comandos internos para exibir a data e a hora em vários formatos. Programadores utilizam essa funcionalidade para tarefas como marcar logs com timestamps, agendar tarefas ou apenas como parte de seus scripts de informação do sistema para rastrear quando as ações foram realizadas.

## Como fazer:
No Bash, o comando `date` é sua principal ferramenta para obter a data e a hora atual. Aqui estão alguns exemplos de como usá-lo:

1. **Obter a data e a hora atual no formato padrão:**

```bash
date
```

*Saída de amostra:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Personalizar o formato de saída:** Você pode especificar o formato de saída usando especificadores de formato `+%`. Por exemplo, para exibir a data no formato AAAA-MM-DD:

```bash
date "+%Y-%m-%d"
```

*Saída de amostra:*
```
2023-04-05
```

3. **Obter o timestamp UNIX atual:** O timestamp UNIX é o número de segundos desde a Época Unix (1 de janeiro de 1970). Isso é útil para scripts que realizam cálculos baseados em diferenças de tempo.

```bash
date "+%s"
```

*Saída de amostra:*
```
1672877344
```

Não são tipicamente usadas bibliotecas de terceiros populares para essa operação básica no Bash, pois o comando `date` integrado oferece funcionalidade abrangente. No entanto, para manipulações de data e hora mais avançadas, programadores podem usar outras linguagens de programação ou ferramentas que oferecem bibliotecas para aritmética e análise de datas, como o módulo `datetime` do Python.
