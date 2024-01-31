---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
YAML é um formato de serialização de dados amigável para humanos, comumente usado para arquivos de configuração. Programadores utilizam YAML pela sua legibilidade e simplicidade, facilitando a definição e compartilhamento de estruturas de dados.

## Como Fazer:
Trabalhar com YAML no Fish Shell geralmente envolve ler e modificar esses arquivo. Vamos usar `yq`, uma ferramenta de linha de comando para processar YAML.

Instale o `yq`:

```Fish Shell
fisher install jorgebucaran/fisher
fisher install PatrickF1/fundle
fundle plugin add mikefarah/yq
```

Exemplo de como ler um valor:

Arquivo `exemplo.yaml`:
```yaml
usuário:
  nome: João
  idade: 30
```

Lendo o nome do usuário:

```Fish Shell
yq e '.usuário.nome' exemplo.yaml
```

Saída esperada:
```
João
```

Mudando a idade do usuário:

```Fish Shell
yq e '.usuário.idade = 31' -i exemplo.yaml
```

Confirmando a alteração:

```Fish Shell
cat exemplo.yaml
```
Saída esperada:
```yaml
usuário:
  nome: João
  idade: 31
```

## Mergulho Profundo
YAML, que significa "YAML Ain't Markup Language", foi introduzido em 2001. Alternativas incluem JSON e XML, mas YAML é preferido por sua ênfase na simplicidade de leitura. A principal implementação de YAML no Fish Shell é feita via `yq`, que é baseado na biblioteca `libyaml`, escrita em C para máxima eficiência.

## Veja Também
- Documentação oficial do `yq`: https://mikefarah.gitbook.io/yq/
- Especificação YAML: https://yaml.org/spec/
- Tutorial interativo de YAML: https://www.learn-yaml.org/
