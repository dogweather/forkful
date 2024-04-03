---
date: 2024-01-26 04:23:12.297447-07:00
description: "TOML significa Tom's Obvious, Minimal Language (Linguagem M\xEDnima\
  \ e \xD3bvia do Tom). \xC9 um formato de serializa\xE7\xE3o de dados usado para\
  \ arquivos de\u2026"
lastmod: '2024-03-13T22:44:46.480250-06:00'
model: gpt-4-0125-preview
summary: "TOML significa Tom's Obvious, Minimal Language (Linguagem M\xEDnima e \xD3\
  bvia do Tom)."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
Você precisará de uma biblioteca de análise (parsing) de TOML. Eu recomendo o `toml4j`. Adicione ao seu projeto assim:

```java
// Adicione isso ao seu build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Veja como você analisa um arquivo TOML:

```java
import com.moandjiezana.toml.Toml;

public class ExemploToml {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("IP do Servidor: " + ip);
        System.out.println("Porta do Servidor: " + port);
    }
}
```

Saída de exemplo:

```
IP do Servidor: 192.168.1.1
Porta do Servidor: 80
```

## Aprofundamento
Desenvolvido por Tom Preston-Werner, co-fundador do GitHub, TOML tinha como objetivo ser mais simples que o XML e mais especificado que o YAML. Sua última versão 1.0.0, lançada em 2021, oferece um conjunto estável de recursos.

Alternativas como JSON ou YAML também são populares. JSON é ótimo para a troca de dados. YAML é mais legível para configurações complexas. A força do TOML é a sua simplicidade e seu uso na comunidade Rust.

Quanto à implementação, ao usar TOML com Java, tenha em mente que o analisador que você escolher importa. Além do `toml4j`, alguns optam pelo `jackson-dataformat-toml`. Eles terão nuances, como manuseio de erros ou desempenho de análise, então escolha com base nas necessidades do seu projeto.

## Veja Também
- Especificação TOML: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
