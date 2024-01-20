---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？
部分文字列を抽出するとは、コード内の特定の文字列の一部を取り出すことです。これは、分析、フィルタリング、変換などのタスクに役立ちます。

## 方法:
```Ruby
s = "こんにちは世界"

s[0,5]     # "こんにちは"
s[5,2]     # "世界"
```
このRubyのコードは部分文字列を抽出します。

## ディープダイブ
部分文字列の抽出はプログラミングの夜明けから存在しており、多くの方法で実装されてきました。Rubyでは、上で述べたようにインデックスと長さを用いて部分文字列を抽出します。これはPythonやJavaなど他の言語でも見かけます。

別のアプローチとしては、正規表現を使った方法もあります。これはパターンマッチングを用いてより複雑な部分文字列の抽出に適しています。

```Ruby
s = "こんにちは世界"

s.match(/こんにちは/)  #<MatchData "こんにちは">
```

## 関連資料
1. Rubyのドキュメンテーションの章、 [String](https://docs.ruby-lang.org/ja/latest/class/String.html)
2. [正規表現について](https://docs.ruby-lang.org/ja/latest/doc/spec=2fregexp.html)
  
これらのリンクは、Rubyの文字列操作と正規表現につながる深い洞察を提供します。