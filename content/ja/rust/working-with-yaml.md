---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

category:             "Rust"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
### 何となぜ？
YAMLはデータ表現のための形式です。わかりやすく、人間にも機械にも扱いやすい。設定ファイルやデータのやり取りに使われる。

## How to:
### どうやって：

RustでYAMLを扱うには、`serde_yaml`クレートを使います。

```Rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: u8,
    activated: bool,
}

fn main() -> serde_yaml::Result<()> {
    // YAMLの文字列
    let config_yaml = "
name: SecretBox
durability: 10
activated: true
";

    // YAMLをデシリアライズ
    let deserialized_config: Config = serde_yaml::from_str(&config_yaml)?;
    println!("{:?}", deserialized_config);

    // オブジェクトをYAMLにシリアライズ
    let serialized_yaml = serde_yaml::to_string(&deserialized_config)?;
    println!("{}", serialized_yaml);

    Ok(())
}
```

出力:

```
Config { name: "SecretBox", durability: 10, activated: true }
---
name: SecretBox
durability: 10
activated: true
```

## Deep Dive
### 詳細な解説：

YAMLは"YAML Ain't Markup Language"の略で可読性を重視。JSONやXMLより人間に優しいが、パーサの複雑さが増す。Rustでは`serde_yaml`を一般的に使うが、`yaml-rust`のような代替クレートもある。処理速度と機能に差があるため、用途に応じて選ぶ。

## See Also
### 参考リンク：

- YAML公式サイト: [https://yaml.org/](https://yaml.org/)
