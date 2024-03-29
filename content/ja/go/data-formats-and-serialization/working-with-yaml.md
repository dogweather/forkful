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
  \u3067\u3042\u308BYAML\uFF08YAML Ain't Markup\u2026"
title: "YAML\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ？

GoでのYAMLの取り扱いには、人間にやさしいデータ直列化標準であるYAML（YAML Ain't Markup Language）ファイルをGoのデータ構造にパースし、その逆の操作を行うことが含まれます。プログラマーは、YAMLのシンプルさと読みやすさを利用して、設定ファイル、アプリケーションの設定、または異なる言語で書かれたサービスやコンポーネント間のデータ交換のためにこれを行います。

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
