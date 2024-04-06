---
date: 2024-01-20 17:50:36.214974-07:00
description: "How to: (\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001\u30C0\u30D6\u30EB\
  \u30AF\u30A9\u30FC\u30C8\u5185\u3067\u76F4\u63A5\u5909\u6570\u3092\u53C2\u7167\u3059\
  \u308B\u3053\u3068\u3067\u6587\u5B57\u5217\u88DC\u9593\u304C\u884C\u308F\u308C\u307E\
  \u3059\u3002\u904E\u53BB\u306E\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u3068\
  \u306F\u7570\u306A\u308A\u3001\u5225\u9014\u69CB\u6587\u3092\u4F7F\u7528\u3059\u308B\
  \u5FC5\u8981\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u4F8B\u3048\u3070\u3001Bash\u3067\
  \u306F\"Hello, ${name}!\"\u3068\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\
  \u304C\u3001Fish\u3067\u306F\u3088\u308A\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.499588-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001\u30C0\u30D6\u30EB\u30AF\u30A9\
  \u30FC\u30C8\u5185\u3067\u76F4\u63A5\u5909\u6570\u3092\u53C2\u7167\u3059\u308B\u3053\
  \u3068\u3067\u6587\u5B57\u5217\u88DC\u9593\u304C\u884C\u308F\u308C\u307E\u3059\u3002\
  \u904E\u53BB\u306E\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u3068\u306F\u7570\
  \u306A\u308A\u3001\u5225\u9014\u69CB\u6587\u3092\u4F7F\u7528\u3059\u308B\u5FC5\u8981\
  \u306F\u3042\u308A\u307E\u305B\u3093\u3002\u4F8B\u3048\u3070\u3001Bash\u3067\u306F\
  \"Hello, ${name}!\"\u3068\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u304C\
  \u3001Fish\u3067\u306F\u3088\u308A\u30B7\u30F3\u30D7\u30EB\u3067\u3059."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
