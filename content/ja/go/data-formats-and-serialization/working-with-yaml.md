---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:00.779358-07:00
description: "Go\u3067\u306EYAML\u306E\u53D6\u308A\u6271\u3044\u306B\u306F\u3001\u4EBA\
  \u9593\u306B\u3084\u3055\u3057\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u6A19\u6E96\
  \u3067\u3042\u308BYAML\uFF08YAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:41.415614-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067\u306EYAML\u306E\u53D6\u308A\u6271\u3044\u306B\u306F\u3001\u4EBA\
  \u9593\u306B\u3084\u3055\u3057\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u6A19\u6E96\
  \u3067\u3042\u308BYAML\uFF08YAML Ain't Markup Language\uFF09\u30D5\u30A1\u30A4\u30EB\
  \u3092Go\u306E\u30C7\u30FC\u30BF\u69CB\u9020\u306B\u30D1\u30FC\u30B9\u3057\u3001\
  \u305D\u306E\u9006\u306E\u64CD\u4F5C\u3092\u884C\u3046\u3053\u3068\u304C\u542B\u307E\
  \u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001YAML\u306E\
  \u30B7\u30F3\u30D7\u30EB\u3055\u3068\u8AAD\u307F\u3084\u3059\u3055\u3092\u5229\u7528\
  \u3057\u3066\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u306E\u8A2D\u5B9A\u3001\u307E\u305F\u306F\u7570\u306A\u308B\
  \u8A00\u8A9E\u3067\u66F8\u304B\u308C\u305F\u30B5\u30FC\u30D3\u30B9\u3084\u30B3\u30F3\
  \u30DD\u30FC\u30CD\u30F3\u30C8\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "YAML\u3068\u306E\u4F5C\u696D"
weight: 41
---

## 方法:
GoでYAMLを扱うには、まず、Goの標準ライブラリにYAMLの直接的なサポートが含まれていないため、YAMLのパースと直列化をサポートするライブラリをインポートする必要があります。「gopkg.in/yaml.v3」はこの目的において最も人気のあるライブラリです。始め方は以下の通りです：

1. **YAMLパッケージのインストール:**

```bash
go get gopkg.in/yaml.v3
```

2. **GoのstructにYAMLをパースする:**

まず、YAMLデータの構造に合ったstructをGoで定義します。

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**サンプル出力:**

```
User: admin
Password: secret
```

3. **YAMLにGoのstructを直列化する：**

ここでは、GoのstructをYAMLに戻す方法を示します。

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**サンプル出力:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## 深堀り:
ソフトウェア開発におけるYAMLの使用は、その人間が読みやすい形式のために増えており、設定ファイル、ドキュメント、またはデータ交換形式に最適な選択肢とされています。その対となるJSONと比較して、YAMLはコメント、スカラータイプ、および関係機能を提供し、より豊かなデータ直列化フレームワークを提供します。しかし、その柔軟性と特徴は、注意深く扱わないと（例えば、任意のコード実行など）、パースの複雑さのコストとなり、潜在的なセキュリティリスクになります。

Go用の「gopkg.in/yaml.v3」ライブラリはYAML処理のための強力な解決策であり、使いやすさと包括的な機能サポートのバランスを取っています。現状では、「go-yaml/yaml」（「gopkg.in/yaml.v3」の背後にあるライブラリ）などの代替手段がありますが、選択されるバージョンは通常、特定のプロジェクト要件や個人の好みに依存します。大規模なデータセットやパフォーマンスが重要なアプリケーションを扱う際には、プログラマーは解析時間とメモリオーバーヘッドが少ないJSONのようなシンプルな形式を検討するかもしれません。それでも、人間の可読性と使いやすさが最優先される設定ファイルや設定においては、YAMLはGoエコシステム内で強力な競争者のままです。
