---
title:                "文字列の長さを求める"
aliases:
- /ja/bash/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:00.284359-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列の長さを見つけるとは、その文字列が含む文字数を数えることです。プログラマーは、入力検証、文字列処理、または指定されたフォーマットの要件を満たすために、これを行います。

## How to: (やり方)
Bashでは文字列の長さを得る方法はいくつかありますが、一番簡単なのは`${#string}`構文を使うことです。

```Bash
string="こんにちは、世界！"
echo ${#string}
```

これの出力は、文字列が何文字かを示します。日本語の場合は少し注意が必要で、マルチバイト文字を正しくカウントするには更なる工夫が要ります。

```Bash
echo "文字列の長さ: ${#string}"
```

出力例：

```
文字列の長さ: 8
```

## Deep Dive (深掘り)
Bashで文字列の長さを得る機能は、プログラミング言語において長く使われている基本的な機能です。`${#string}`構文は読みやすく、複雑な操作を必要としない直感的な方法です。しかしながら、マルチバイト文字対応には`wc`コマンドを利用したり、`iconv`や`mbstring`を使ったりと、異なるアプローチがあります。

```Bash
# マルチバイト対応
string="こんにちは、世界！"
echo "バイト数: $(echo -n $string | wc -c)"
echo "文字数: $(echo -n $string | wc -m)"
```

他の言語だと、例えばPythonでは`len()`関数を使い、Rubyでは`.length`や`.size`メソッドを使います。これらが提供する文字列の長さの取得機能も同様によく利用されます。

## See Also (参照)
- Bash のマニュアル: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- `wc` コマンドの詳細: http://man7.org/linux/man-pages/man1/wc.1.html
- 文字列の長さを得るための別のスクリプトや言語に関する Stack Overflow の質問と議論: https://stackoverflow.com/search?q=string+length
