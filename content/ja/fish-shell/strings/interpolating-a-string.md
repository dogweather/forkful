---
aliases:
- /ja/fish-shell/interpolating-a-string/
date: 2024-01-20 17:50:36.214974-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u5909\u6570\u306E\u5024\u3092\
  \u6587\u5B57\u5217\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u3092\u52D5\u7684\u306B\
  \u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3057\u3001\u51FA\u529B\u3092\u67D4\u8EDF\u306B\
  \u5236\u5FA1\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.294667
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u5909\u6570\u306E\u5024\u3092\
  \u6587\u5B57\u5217\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u3092\u52D5\u7684\u306B\
  \u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3057\u3001\u51FA\u529B\u3092\u67D4\u8EDF\u306B\
  \u5236\u5FA1\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは変数の値を文字列中に埋め込むことです。プログラマーはコードを動的にカスタマイズし、出力を柔軟に制御するためにこれを行います。

## How to: (方法)
```Fish Shell
# 変数を定義
set name "World"

# 文字列補間を使用して変数を埋め込む
echo "Hello, $name!"

# 出力: Hello, World!
```

## Deep Dive (深掘り)
Fish Shellでは、ダブルクォート内で直接変数を参照することで文字列補間が行われます。過去のシェルスクリプトとは異なり、別途構文を使用する必要はありません。例えば、Bashでは"Hello, ${name}!"とする必要がありますが、Fishではよりシンプルです。

また、Fish Shellの補間はリアルタイムで行われ、実行時に変数の現在の値を取得します。これにより動的なスクリプトが書きやすくなっています。

## See Also (関連項目)
- [Fish Shell Documentation - Variables](https://fishshell.com/docs/current/#variables)
- [Fish Shell Documentation - Quotes](https://fishshell.com/docs/current/#quotes)
