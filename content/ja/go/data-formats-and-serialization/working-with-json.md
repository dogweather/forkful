---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:25.158533-07:00
description: "Go\u3067JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:41.416463-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067JSON\uFF08JavaScript Object Notation\uFF09\u3092\u6271\u3046\u3053\
  \u3068\u306F\u3001Go\u306E\u30C7\u30FC\u30BF\u69CB\u9020\u3068JSON\u5F62\u5F0F\u306E\
  \u9593\u3067\u30C7\u30FC\u30BF\u3092\u30A8\u30F3\u30B3\u30FC\u30C9\u304A\u3088\u3073\
  \u30C7\u30B3\u30FC\u30C9\u3059\u308B\u4F5C\u696D\u3092\u542B\u307F\u307E\u3059\u3002\
  \u3053\u306E\u4F5C\u696D\u306F\u3001JSON\u304C\u8EFD\u91CF\u3067\u3001\u30C6\u30AD\
  \u30B9\u30C8\u30D9\u30FC\u30B9\u3067\u3001\u8A00\u8A9E\u306B\u4F9D\u5B58\u3057\u306A\
  \u3044\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u3057\
  \u3066\u6A5F\u80FD\u3057\u3001\u7570\u306A\u308B\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\
  \u30B0\u74B0\u5883\u9593\u3067\u30B7\u30F3\u30D7\u30EB\u306B\u30C7\u30FC\u30BF\u5171\
  \u6709\u3092\u53EF\u80FD\u306B\u3059\u308B\u305F\u3081\u3001Web\u30B5\u30FC\u30D3\
  \u30B9\u3084API\u3067\u81F3\u308B\u6240\u3067\u898B\u3089\u308C\u307E\u3059\u3002\
  ."
title: "JSON\u3092\u5229\u7528\u3059\u308B"
weight: 38
---

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
