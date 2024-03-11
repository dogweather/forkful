---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:25.158533-07:00
description: "Go\u3067JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-11T00:14:15.016195-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067JSON\uFF08JavaScript Object\u2026"
title: "JSON\u3092\u5229\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

GoでJSON（JavaScript Object Notation）を扱うことは、Goのデータ構造とJSON形式の間でデータをエンコードおよびデコードする作業を含みます。この作業は、JSONが軽量で、テキストベースで、言語に依存しないデータ交換フォーマットとして機能し、異なるプログラミング環境間でシンプルにデータ共有を可能にするため、WebサービスやAPIで至る所で見られます。

## 方法:

Goでは、`encoding/json` パッケージがJSONの操作への入口となり、Goのデータ構造をJSONに変換（マーシャリング）する機構と、その逆の変換（アンマーシャリング）を提供します。以下は、始めるための基本的な例です：

### エンコード（マーシャリング）

GoのstructをJSONに変換するには、`json.Marshal`を使用できます。次のGoのstructを考えてみましょう：

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

出力：

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### デコード（アンマーシャリング）

Goのデータ構造にJSONを解析するには、`json.Unmarshal`を使います：

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

上記の`User`構造体を用いて、このコードはJSON文字列をUserインスタンスに解析します。

出力：

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## 深堀り

Goの`encoding/json`パッケージは、JSONの操作における多くの複雑さを抽象化する直感的なAPIを提供しています。Goの開発初期に導入されたこのパッケージは、Goのシンプルさと効率性の哲学を反映しています。しかし、`encoding/json`がランタイムで構造体を検査し変更する反射を使用すると、CPU集約的なシナリオでは最適でないパフォーマンスにつながる可能性があります。

`json-iterator/go`や`ffjson`のような代替手段が登場し、静的なマーシャリングおよびアンマーシャリングコードを生成することで、より速いJSON処理を提供しています。しかし、`encoding/json`はそのシンプルさ、堅牢性、そして標準ライブラリの一部であるという事実により、最も一般的に使用されるパッケージのままです。これにより、Goのバージョン間での互換性と安定性が保証されます。

比較的遅いパフォーマンスにもかかわらず、使いやすさとGoの型システムとの統合は、ほとんどのアプリケーションにとって`encoding/json`を適しています。パフォーマンスが最優先のコンテキストで作業する人々にとって、外部ライブラリの探索が価値があるかもしれませんが、多くの人にとって、標準ライブラリは速度、シンプルさ、および信頼性の間の正しいバランスを提供します。
