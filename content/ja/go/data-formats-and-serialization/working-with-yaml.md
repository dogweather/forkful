---
title:                "YAMLとの作業"
date:                  2024-02-03T18:14:00.779358-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAMLとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
