---
date: 2024-01-26 01:08:45.825534-07:00
description: "Registrar logs \xE9 como manter um di\xE1rio para a sua aplica\xE7\xE3\
  o; \xE9 a pr\xE1tica de gravar eventos, erros e outros dados pertinentes durante\
  \ a execu\xE7\xE3o.\u2026"
lastmod: '2024-03-11T00:14:20.065121-06:00'
model: gpt-4-1106-preview
summary: "Registrar logs \xE9 como manter um di\xE1rio para a sua aplica\xE7\xE3o;\
  \ \xE9 a pr\xE1tica de gravar eventos, erros e outros dados pertinentes durante\
  \ a execu\xE7\xE3o.\u2026"
title: Registro de Logs
---

{{< edit_this_page >}}

## O Quê & Porquê?

Registrar logs é como manter um diário para a sua aplicação; é a prática de gravar eventos, erros e outros dados pertinentes durante a execução. Desenvolvedores utilizam logs para diagnosticar problemas, monitorar o comportamento do sistema e reunir informações que impulsionam melhorias – é a base da inteligência operacional.

## Como fazer:

Vamos configurar um cenário básico de registro de logs em Rust, utilizando a crate `log`, que fornece uma fachada de logs, e `env_logger`, uma implementação de logs para a crate `log`. Primeiro, adicione-as ao seu Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Agora, configure e inicialize o logger no seu `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Esta é uma mensagem de informação.");
    warn!("Esta é uma mensagem de aviso.");
}
```

Execute sua aplicação com `RUST_LOG=info cargo run`, e você verá a saída:

```
INFO: Esta é uma mensagem de informação.
WARN: Esta é uma mensagem de aviso.
```

Experimente com a variável de ambiente `RUST_LOG` configurando-a para `error`, `warn`, `info`, `debug` ou `trace` para controlar a verbosidade dos seus logs.

## Aprofundamento

O conceito de registro de logs não é algo novo; ele existe desde os primórdios da computação. Antes do registro de logs ser comum no software, desenvolvedores dependiam de métodos primitivos, como instruções de impressão ou ferramentas de depuração para rastrear a execução de programas. À medida que os programas aumentavam em complexidade, crescia também a necessidade de abordagens estruturadas para o registro de logs.

Em Rust, a crate `log` abstrai os detalhes da implementação do registro de logs, permitindo aos desenvolvedores conectar diferentes backends de logs. Enquanto `env_logger` é uma escolha comum, existem alternativas como `fern`, `slog` ou `tracing`, cada uma com seu próprio conjunto de características e opções de configuração.

Algumas considerações ao implementar registro de logs incluem:

1. **Níveis de Log**: Controlar a verbosidade é essencial. A crate `log` do Rust define vários níveis de logs: error, warn, info, debug e trace, em ordem decrescente de severidade.

2. **Desempenho**: Registrar logs pode afetar o desempenho. É crítico usar isso com prudência, assegurando-se de evitar registros em caminhos críticos de desempenho ou logs excessivamente verbosos em produção.

3. **Registro de Logs Estruturado**: As melhores práticas modernas envolvem o registro de logs estruturado, onde os logs são escritos em um formato legível por máquina, como JSON. Bibliotecas como `slog` permitem o registro de logs estruturados em Rust, que podem ser indexados e consultados usando sistemas de gerenciamento de logs como ELK Stack ou Splunk.

4. **Registro de Logs Assíncrono**: Para minimizar o impacto na aplicação principal, o registro de logs pode ser realizado de forma assíncrona. Isso é frequentemente alcançado ao fazer a biblioteca de logs escrever em uma fila na memória, e uma thread separada processa a fila e escreve os logs no destino.

5. **Configuração**: Muitas estruturas de logs suportam configuração por meio de variáveis de ambiente, arquivos de configuração e/ou código. Essa flexibilidade é chave para ajustar a saída em diferentes ambientes (desenvolvimento, teste, produção).

## Veja Também

- A documentação da crate `log`: https://docs.rs/log/
- A documentação da crate `env_logger`: https://docs.rs/env_logger/
- A página de registro de logs em Rust by Example: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- A crate `slog`, uma estrutura de logs alternativa: https://github.com/slog-rs/slog
- Tracing, uma estrutura para instrumentar programas Rust: https://crates.io/crates/tracing
