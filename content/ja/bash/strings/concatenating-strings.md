---
date: 2024-01-20 17:34:22.512390-07:00
description: "\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\u308B\u3068\u306F\u3001\u3044\
  \u304F\u3064\u304B\u306E\u6587\u5B57\u5217\u3092\u3064\u306A\u3052\u3066\u4E00\u3064\
  \u306E\u9577\u3044\u6587\u5B57\u5217\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\u7406\
  \u3084\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u69CB\u7BC9\u3001\u3042\u308B\u3044\u306F\
  \u30D5\u30A1\u30A4\u30EB\u30D1\u30B9\u306A\u3069\u3092\u4F5C\u6210\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.486031
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\u308B\u3068\u306F\u3001\u3044\
  \u304F\u3064\u304B\u306E\u6587\u5B57\u5217\u3092\u3064\u306A\u3052\u3066\u4E00\u3064\
  \u306E\u9577\u3044\u6587\u5B57\u5217\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\u7406\
  \u3084\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u69CB\u7BC9\u3001\u3042\u308B\u3044\u306F\
  \u30D5\u30A1\u30A4\u30EB\u30D1\u30B9\u306A\u3069\u3092\u4F5C\u6210\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (なにを？そしてなぜ？)

文字列を連結するとは、いくつかの文字列をつなげて一つの長い文字列にすることです。プログラマーは、データの整理やメッセージの構築、あるいはファイルパスなどを作成するためにこれを行います。

## How to: (やり方)

```Bash
# 直接つなげる
string1="Hello,"
string2=" World!"
greeting=$string1$string2
echo $greeting # 出力: Hello, World!

# 変数を使わずにつなげる
echo "Concatenating " "strings " "is " "fun!" # 出力: Concatenating strings is fun!

# {}を使って明確にする
name="Taro"
echo "Your name is ${name}san." # 出力: Your name is Tarosan.
```

## Deep Dive (深掘り)

最初に、Bashによる文字列の連結は非常に単純です。1989年に登場したBashは、Unixシェルの伝統を引き継ぎつつ、プログラミングの簡単さを改善しました。今日でも、このシンプルな連結はスクリプトの読みやすさと保守性に貢献しています。

他の方法として、`printf` や `echo` などのコマンドでも文字列を連結できますが、変数だけで連結する方がシンプルで効率的です。演算子や関数を使用せずとも、Bashは変数を直接つなげることで文字列を連結させます。内部実装では、Bashは文字列を連結する際に新しいメモリ領域を割り当て、元の文字列を新しい領域にコピーしています。

## See Also (関連情報)

- Bash String Manipulation: [https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- Bash Programming Introduction HOWTO: [https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
