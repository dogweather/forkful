---
aliases:
- /ja/ruby/printing-debug-output/
date: 2024-01-20 17:53:47.525459-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u30D0\u30B0\u3092\u898B\u3064\u3051\
  \u308B\u305F\u3081\u3001\u30B3\u30FC\u30C9\u5B9F\u884C\u6642\u306E\u5909\u6570\u3084\
  \u7D50\u679C\u3092\u8868\u793A\u3059\u308B\u306E\u304C\u30C7\u30D0\u30C3\u30B0\u51FA\
  \u529B\u3067\u3059\u3002\u306A\u305C\u3084\u308B\u306E\uFF1F\u30A8\u30E9\u30FC\u3084\
  \u4E88\u671F\u305B\u306C\u6319\u52D5\u306E\u539F\u56E0\u3092\u63A2\u308A\u3001\u7D20\
  \u65E9\u304F\u5BFE\u51E6\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.393732
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u30D0\u30B0\u3092\u898B\u3064\u3051\
  \u308B\u305F\u3081\u3001\u30B3\u30FC\u30C9\u5B9F\u884C\u6642\u306E\u5909\u6570\u3084\
  \u7D50\u679C\u3092\u8868\u793A\u3059\u308B\u306E\u304C\u30C7\u30D0\u30C3\u30B0\u51FA\
  \u529B\u3067\u3059\u3002\u306A\u305C\u3084\u308B\u306E\uFF1F\u30A8\u30E9\u30FC\u3084\
  \u4E88\u671F\u305B\u306C\u6319\u52D5\u306E\u539F\u56E0\u3092\u63A2\u308A\u3001\u7D20\
  \u65E9\u304F\u5BFE\u51E6\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムのバグを見つけるため、コード実行時の変数や結果を表示するのがデバッグ出力です。なぜやるの？エラーや予期せぬ挙動の原因を探り、素早く対処するためです。

## How to: (やり方)
Rubyでデバッグ出力するには、`puts` や `p` を使用します。`p` は `.inspect` メソッドを呼び出し、より詳細な情報を出力します。

```Ruby
# putsを使う
puts "これはデバッグのメッセージです"

# 変数の内容を出力
x = 42
puts "変数xの値: #{x}"

# 配列を出力
arr = [1, 2, 3]
puts "配列arrの内容: #{arr}"

# pを使用すると、より多くの情報が得られる
p arr
```

出力例:
```
これはデバッグのメッセージです
変数xの値: 42
配列arrの内容: [1, 2, 3]
[1, 2, 3]
```

`p` を使用した場合、出力には配列の括弧が含まれます。これにより、データの型をより明確に確認できます。

## Deep Dive (深掘り)
デバッグ出力は、Rubyだけではなくほぼ全てのプログラミング言語で基本的な機能です。過去には、デバッグ情報は紙のプリントアウトにされることもありましたが、今はコンソールやログファイルに出力されます。 

`puts` と `p` の他に、`pp`（pretty print）メソッドもあり、複雑なオブジェクトを見やすく表示します。時には、標準ライブラリの `Logger` クラスを使って、出力レベルに応じてログを管理することも重要です。

Ruby の実装においては、`IO` クラスが出力を取り扱っており、`$stdout` というグローバル変数で標準出力を参照しています。

## See Also (関連情報)
- [RubyのIOクラス](https://docs.ruby-lang.org/ja/latest/class/IO.html)
- [ログ出力のための Logger クラス](https://docs.ruby-lang.org/ja/latest/library/logger.html)
