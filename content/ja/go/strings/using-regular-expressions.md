---
aliases:
- /ja/go/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:35.634475-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6B63\
  \u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\
  \u30F3\u306B\u57FA\u3065\u3044\u3066\u6587\u5B57\u5217\u3092\u691C\u7D22\u3001\u4E00\
  \u81F4\u3055\u305B\u3001\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\
  \u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\u30F3\
  \u30D7\u30EB\u306A\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u30C1\u30A7\u30C3\u30AF\
  \u304B\u3089\u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u307E\u3067\u3001\
  \u5E45\u5E83\u3044\u30BF\u30B9\u30AF\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u3001\u30C6\u30AD\u30B9\u30C8\u3092\u67D4\u8EDF\u304B\u3064\u52B9\u7387\u7684\u306B\
  \u6271\u3046\u305F\u3081\u306B\u6B20\u304B\u305B\u306A\u3044\u3082\u306E\u3068\u3057\
  \u3066\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.469455
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6B63\
  \u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\
  \u30F3\u306B\u57FA\u3065\u3044\u3066\u6587\u5B57\u5217\u3092\u691C\u7D22\u3001\u4E00\
  \u81F4\u3055\u305B\u3001\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\
  \u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\u30F3\
  \u30D7\u30EB\u306A\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u30C1\u30A7\u30C3\u30AF\
  \u304B\u3089\u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u307E\u3067\u3001\
  \u5E45\u5E83\u3044\u30BF\u30B9\u30AF\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u3001\u30C6\u30AD\u30B9\u30C8\u3092\u67D4\u8EDF\u304B\u3064\u52B9\u7387\u7684\u306B\
  \u6271\u3046\u305F\u3081\u306B\u6B20\u304B\u305B\u306A\u3044\u3082\u306E\u3068\u3057\
  \u3066\u3044\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングにおける正規表現（regex）は、特定のパターンに基づいて文字列を検索、一致させ、操作するために使用されます。プログラマーは、シンプルなバリデーションチェックから複雑なテキスト処理まで、幅広いタスクにそれらを使用し、テキストを柔軟かつ効率的に扱うために欠かせないものとしています。

## 使い方:

Goでは、`regexp`パッケージが正規表現の機能を提供しています。以下にその使用方法のステップバイステップガイドを示します：

1. **正規表現をコンパイルする**

まず、`regexp.Compile`を使って正規表現パターンをコンパイルします。コンパイル中に発生する可能性のあるエラーを処理することは良い習慣です。

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    pattern := "go+"
    r, err := regexp.Compile(pattern)
    if err != nil {
        fmt.Println("正規表現のコンパイル中にエラーが発生しました:", err)
        return
    }
    
    fmt.Println("正規表現のコンパイルに成功しました")
}
```

2. **文字列の一致をチェックする**

`MatchString`メソッドを使用して、文字列がパターンと一致するかどうかをチェックします。

```go
matched := r.MatchString("goooooogle")
fmt.Println("一致しました:", matched) // 出力: 一致しました: true
```

3. **一致を見つける**

文字列の最初の一致を見つけるために、`FindString`メソッドを使用します。

```go
match := r.FindString("golang gooooo")
fmt.Println("見つかりました:", match) // 出力: 見つかりました: gooooo
```

4. **すべての一致を見つける**

すべての一致を見つけるために、`FindAllString`は入力文字列と整数nを取ります。n >= 0の場合、最大n個の一致を返します。n < 0の場合、すべての一致を返します。

```go
matches := r.FindAllString("go gooo gooooo", -1)
fmt.Println("すべての一致:", matches) // 出力: すべての一致: [go gooo gooooo]
```

5. **一致を置き換える**

一致を別の文字列で置き換えるには、`ReplaceAllString`が便利です。

```go
result := r.ReplaceAllString("go gooo gooooo", "Java")
fmt.Println("置き換えました:", result) // 出力: 置き換えました: Java Java Java
```

## より深く

Goの標準ライブラリに導入された`regexp`パッケージは、Perlの構文に触発された正規表現の検索とパターンマッチングを実装しています。その内部では、Goの正規表現エンジンがパターンをバイトコードの形式にコンパイルし、それがGo自身で書かれたマッチングエンジンによって実行されます。この実装は、直接ハードウェア実行におけるいくらかの速度を安全性と使いやすさのためにトレードオフし、Cベースのライブラリで一般的なバッファオーバーランの落とし穴を避けます。

その力にもかかわらず、JSONやXMLなどの高度に構造化されたデータを扱う場合、特にGoにおいて正規表現がパターンマッチングの最適な解決策であるとは限りません。これらのケースでは、これらのデータ形式に特化したパーサーやライブラリが、より良いパフォーマンスと信頼性を提供します。それでも、定義された構造なしに複雑なテキスト処理を含むタスクに関しては、正規表現はプログラマーのツールキットにおいて不可欠なツールとして残り、ごくわずかな代替手段では匹敵しない力と柔軟性のバランスを提供します。
