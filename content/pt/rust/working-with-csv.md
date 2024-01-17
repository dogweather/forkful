---
title:                "Trabalhando com csv"
html_title:           "Rust: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-csv.md"
---

{{< edit_this_page >}}

O que & Por quê?

Trabalhar com CSV significa manipular dados em um arquivo de "valores separados por vírgula". Os programadores frequentemente lidam com arquivos CSV para importar, exportar e analisar grandes quantidades de dados de forma simples e eficiente.

Como fazer:

Para trabalhar com CSV em Rust, primeiro você precisa adicionar a dependência "csv" ao seu arquivo Cargo.toml. Em seguida, use a biblioteca "csv" para ler ou escrever em um arquivo CSV. Por exemplo:

```
use csv;

// Lendo dados de um arquivo CSV e imprimindo-os na tela
let mut reader = csv::Reader::from_path("dados.csv")?;
for result in reader.records() {
    let record = result?;
    println!("{:?}", record);
}

// Escrevendo dados em um arquivo CSV
let mut writer = csv::Writer::from_path("dados.csv")?;
writer.write_record(&["Nome", "Idade", "Cidade", "Profissão"])?;
writer.write_record(&["João", "30", "São Paulo", "Engenheiro"])?;
```

Deep Dive:

CSV é um formato de arquivo amplamente utilizado para armazenar dados em formato de tabela. Ele foi criado nos anos 70 como uma maneira fácil de compartilhar dados entre diferentes sistemas e programas. Embora simples, o formato CSV pode ser encontrado em diversas variações, então é importante entender a formatação específica ao lidar com diferentes arquivos.

Além da biblioteca "csv", existem outras ferramentas em Rust que podem ser úteis para trabalhar com CSV, como por exemplo a biblioteca "serde", que permite a serialização e desserialização de dados em vários formatos, incluindo CSV.

Veja também:

- Documentação da biblioteca csv: [https://docs.rs/csv/1.0.0/csv/index.html](https://docs.rs/csv/1.0.0/csv/index.html)
- Biblioteca serde: [https://github.com/serde-rs/serde](https://github.com/serde-rs/serde)