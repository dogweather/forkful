---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:25.158533-07:00
description: "\u65B9\u6CD5: Go\u3067\u306F\u3001`encoding/json` \u30D1\u30C3\u30B1\
  \u30FC\u30B8\u304CJSON\u306E\u64CD\u4F5C\u3078\u306E\u5165\u53E3\u3068\u306A\u308A\
  \u3001Go\u306E\u30C7\u30FC\u30BF\u69CB\u9020\u3092JSON\u306B\u5909\u63DB\uFF08\u30DE\
  \u30FC\u30B7\u30E3\u30EA\u30F3\u30B0\uFF09\u3059\u308B\u6A5F\u69CB\u3068\u3001\u305D\
  \u306E\u9006\u306E\u5909\u63DB\uFF08\u30A2\u30F3\u30DE\u30FC\u30B7\u30E3\u30EA\u30F3\
  \u30B0\uFF09\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u59CB\
  \u3081\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\uFF1A #."
lastmod: '2024-03-13T22:44:41.416463-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067\u306F\u3001`encoding/json` \u30D1\u30C3\u30B1\u30FC\u30B8\u304C\
  JSON\u306E\u64CD\u4F5C\u3078\u306E\u5165\u53E3\u3068\u306A\u308A\u3001Go\u306E\u30C7\
  \u30FC\u30BF\u69CB\u9020\u3092JSON\u306B\u5909\u63DB\uFF08\u30DE\u30FC\u30B7\u30E3\
  \u30EA\u30F3\u30B0\uFF09\u3059\u308B\u6A5F\u69CB\u3068\u3001\u305D\u306E\u9006\u306E\
  \u5909\u63DB\uFF08\u30A2\u30F3\u30DE\u30FC\u30B7\u30E3\u30EA\u30F3\u30B0\uFF09\u3092\
  \u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u59CB\u3081\u308B\u305F\
  \u3081\u306E\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\uFF1A\n\n#."
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
