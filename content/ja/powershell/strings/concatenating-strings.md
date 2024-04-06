---
date: 2024-01-20 17:35:21.056144-07:00
description: "How to: (\u65B9\u6CD5) PowerShell\u306F\u6587\u5B57\u5217\u3092\u9023\
  \u7D50\u3059\u308B\u7C21\u5358\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.247813-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PowerShell\u306F\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\
  \u308B\u7C21\u5358\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
