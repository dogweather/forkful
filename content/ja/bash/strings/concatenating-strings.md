---
title:                "文字列の連結"
date:                  2024-01-20T17:34:22.512390-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/concatenating-strings.md"
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
