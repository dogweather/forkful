---
date: 2024-01-20 17:50:18.985885-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u5909\u6570\u3084\u5F0F\u306E\
  \u7D50\u679C\u3092\u6587\u5B57\u5217\u306B\u57CB\u3081\u8FBC\u3080\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u306E\u53EF\
  \u8AAD\u6027\u3092\u4E0A\u3052\u305F\u308A\u3001\u52D5\u7684\u306A\u30E1\u30C3\u30BB\
  \u30FC\u30B8\u3092\u4F5C\u3063\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.331441-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u5909\u6570\u3084\u5F0F\u306E\
  \u7D50\u679C\u3092\u6587\u5B57\u5217\u306B\u57CB\u3081\u8FBC\u3080\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u306E\u53EF\
  \u8AAD\u6027\u3092\u4E0A\u3052\u305F\u308A\u3001\u52D5\u7684\u306A\u30E1\u30C3\u30BB\
  \u30FC\u30B8\u3092\u4F5C\u3063\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは変数や式の結果を文字列に埋め込むことです。プログラマーはコードの可読性を上げたり、動的なメッセージを作ったりするためにこれを行います。

## How to: (実践方法)
```Bash
# 変数に値を割り当てます
greeting="こんにちは"
name="世界"

# 文字列補間を使って変数を出力します
echo "${greeting}、${name}！"

# 数式の結果も埋め込むことができます
a=5
b=10
echo "合計は: $(($a + $b))"
```
出力:
```
こんにちは、世界！
合計は: 15
```

## Deep Dive (掘り下げ)
文字列補間は古くからシェルスクリプトにおける基本機能で、効率的なコード作成に役立ちます。Bash以前は`expr`コマンドを使用しますが、今では`$()`や`${}`構文が使われています。これらは子プロセスを作らずに処理できるため効率的です。代わりにダブルクオートを使うと、変数やコマンド出力が展開されません。

## See Also (参照)
- Bash Manual: [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- Advanced Bash-Scripting Guide: [ABS Guide](https://www.tldp.org/LDP/abs/html/)
