---
title:                "文字列の連結"
aliases:
- ja/powershell/concatenating-strings.md
date:                  2024-01-20T17:35:21.056144-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字列の連結とは、2つ以上の文字列をつなげて1つの文字列にすることです。プログラマーがこれを行う理由は、動的なテキストを生成したり、データを整形する場合が多いです。

## How to: (方法)

PowerShellは文字列を連結する簡単な方法を提供しています。以下に例を示します。

```PowerShell
# + 演算子を使って連結
$string1 = "PowerShell"
$string2 = "は楽しい！"
$result = $string1 + " " + $string2
$result  # 出力: PowerShell は楽しい！

# テンプレートリテラルを使って変数を埋め込む
$name = "世界"
$greeting = "こんにちは, $name!"
$greeting  # 出力: こんにちは, 世界!

# -join 演算子を使用
$words = "PowerShell", "が", "大好き！"
$phrase = $words -join " "
$phrase  # 出力: PowerShell が 大好き！
```

## Deep Dive (深堀り)

文字列の連結は、古くからある基本的な操作です。しかし、実装方法によってパフォーマンスが異なる場合があります。大規模なデータを連結するときは、`StringBuilder` クラスを利用するとメモリ使用が効率的です。

PowerShellでの +-演算子はシンプルですが、いくつかのピットフォールがあります。連結する文字列が多い場合、パフォーマンスが落ちることがあります。特にループの内部で連結を大量に行うときは注意が必要です。

別の選択肢として `-f` フォーマット演算子や、ヒアストリング (here-strings) を使う方法があります。これらの方法はより複雑な構文を提供し、複数行にわたる文字列やフォーマット済みのテキストの連結に便利です。

## See Also (関連情報)

- [Microsoft の PowerShell ドキュメント](https://docs.microsoft.com/ja-jp/powershell/)
- [`StringBuilder` クラスの利用法](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.stringbuilder?view=net-5.0)
