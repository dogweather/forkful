---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:01.160964-07:00
description: "TOML\uFF08Tom's Obvious, Minimal\u2026"
lastmod: '2024-03-13T22:44:41.419077-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal\u2026"
title: "TOML\u3092\u64CD\u4F5C\u3059\u308B"
weight: 39
---

## 何となく & なぜ？

TOML（Tom's Obvious, Minimal Language）は、シンプルな構文のおかげで読みやすい設定ファイル形式です。プログラマーは、その明瞭さとデータ構造への直接的なマッピングのおかげで、アプリケーションの設定と依存関係の設定にTOMLを使用します。これは、配置と設定の管理において、多くのGoプロジェクトで人気の選択肢になっています。

## どのようにして：

GoでTOMLを使い始めるには、まず、Go標準ライブラリがTOMLをネイティブにサポートしていないため、TOMLファイルを解析できるライブラリを含める必要があります。`BurntSushi/toml`パッケージは、この目的にために人気の選択肢です。まず、これをインストールしてください：

```bash
go get github.com/BurntSushi/toml
```

これがそれを使用する簡単な例です。次の内容を持つ`config.toml`という設定ファイルがあるとします：

```toml
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

次に、TOML構造を反映するGoの構造体を作成する必要があります：

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

サンプル出力：

```
Title: TOML Example
Database Server: 192.168.1.1
```

## 詳細分析

TOMLは、GitHubの共同創設者の一人であるTom Preston-Wernerによって作成されました。これは、ハッシュテーブルに簡単にマッピングでき、形式について事前の知識がなくても一目で理解できる直接的な設定ファイル形式を提供するためです。これは、ブレース、引用符、およびインデントの問題のために、設定ファイルにとって人に優しくない可能性があるJSONやYAMLと対照的です。

Goの`BurntSushi/toml`パッケージは、デコードだけでなく、TOMLファイルのエンコーディングも可能にする頑健なライブラリであり、この形式で設定ファイルを読み書きする必要があるアプリケーションにとって多用途な選択肢です。ただし、技術の進歩と新しいGoバージョンの導入に伴い、`pelletier/go-toml`などの代替手段が登場し、パフォーマンスの向上や木構造の操作、クエリサポートなどの追加機能を提供しています。

TOMLは多くのアプリケーションにとって優れた選択肢ですが、アプリケーションの設定の複雑さや個人またはチームの好みに応じて、YAMLやJSONなどの他の形式の方が適している場合もあります。特に、TOMLの冗長な性質がエレガントにキャプチャできないより複雑なデータ構造が必要な場合です。それでも、直接的で読みやすく、簡単に編集できる設定には、TOMLとGoの強力な型システムおよび上述のライブラリを組み合わせることが優れた選択です。
