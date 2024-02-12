---
title:                "Trabalhando com YAML"
aliases:
- /pt/fish-shell/working-with-yaml.md
date:                  2024-02-03T19:25:26.075905-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com YAML envolve analisar e manipular arquivos YAML (YAML Ain't Markup Language), um formato de serialização de dados usado para arquivos de configuração, no Fish Shell. Programadores fazem isso para automatizar e configurar aplicações ou serviços de forma eficiente dentro do contexto de ambientes shell, facilitando tarefas como gerenciamento de configurações e provisionamento de recursos.

## Como fazer:
O Fish Shell não tem suporte nativo para analisar YAML, mas você pode utilizar ferramentas de terceiros como o `yq` (um processador de linha de comando YAML leve e portátil) para manipular dados YAML.

**Instalação do yq (se ainda não estiver instalado):**
```fish
sudo apt-get install yq
```

**Lendo um valor de um arquivo YAML:**
Suponha que você tenha um arquivo YAML `config.yaml` com o seguinte conteúdo:
```yaml
database:
  host: localhost
  port: 3306
```

Para ler o host do banco de dados, você usaria:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Saída de exemplo:**
```
localhost
```

**Atualizando um valor em um arquivo YAML:**
Para atualizar o `port` para `5432`, use:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Verifique a atualização:**
```fish
yq e '.database.port' config.yaml
```
**Saída de exemplo:**
```
5432
```

**Escrevendo um novo arquivo YAML:**
Para criar um novo `new_config.yaml` com conteúdo predefinido:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Isso usa o `yq` para processar e imprimir de forma bonita (-P flag) uma string em um novo arquivo YAML.

**Analisando estruturas complexas:**
Se você tem um arquivo YAML mais complexo e precisa buscar arrays ou objetos aninhados, você pode:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Saída de exemplo:**
```
server1
server2
```
Usando o `yq`, o Fish Shell torna simples navegar por documentos YAML e manipulá-los para várias tarefas de automação e configuração.
