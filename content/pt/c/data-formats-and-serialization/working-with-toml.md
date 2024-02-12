---
title:                "Trabalhando com TOML"
date:                  2024-02-03T18:12:25.272408-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

TOML (Tom's Obvious, Minimal Language) é um formato de arquivo de configuração fácil de ler devido à sua clareza semântica. Programadores o utilizam em arquivos de configuração de aplicações porque sua simplicidade e legibilidade humana o tornam uma escolha excelente sobre formatos como XML ou JSON em certos contextos.

## Como Fazer:

Para trabalhar com TOML em C, você primeiro precisa de uma biblioteca capaz de analisar arquivos TOML, já que a biblioteca padrão do C não inclui essa funcionalidade. Uma escolha popular é `tomlc99`, um analisador TOML leve para C99. Aqui está um guia rápido para ler um simples arquivo de configuração TOML:

Primeiro, certifique-se de ter instalado e devidamente vinculado o `tomlc99` no seu projeto.

**Arquivo TOML de exemplo (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Código C para analisar este arquivo:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Não é possível abrir o arquivo");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Erro ao analisar arquivo\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Servidor de Banco de Dados: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Porta %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Saída:**
```
Servidor de Banco de Dados: "192.168.1.1"
Porta 0: 8001
Porta 1: 8001
Porta 2: 8002
```

## Aprofundamento

O TOML foi criado por Tom Preston-Werner, co-fundador do GitHub, como resposta às limitações que percebeu em outros formatos de arquivo de configuração. Seu objetivo é ser direto e inequívoco, tanto para humanos quanto para computadores, ao ler e escrever sem precisar de regras de análise complexas. No ecossistema C, TOML não é um cidadão de primeira classe como pode ser em linguagens de alto nível, como Rust com seu `serde_toml` ou Python com `toml`, que possuem bibliotecas com suporte nativo. Em vez disso, desenvolvedores C precisam contar com bibliotecas externas como `tomlc99`, mas isso é típico dada a ênfase do C em minimalismo e desempenho.

Embora o TOML seja elogiado por sua clareza, ao escolher um formato de arquivo de configuração, é vital considerar as necessidades do projeto. Em cenários que exigem estruturas mais complexas ou interatividade com APIs da web, JSON ou mesmo YAML podem oferecer um melhor ajuste apesar de sua complexidade aumentada. TOML brilha em configurações onde a legibilidade e simplicidade são primordiais, não necessariamente onde as estruturas de dados mais avançadas são necessárias.
