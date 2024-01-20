---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 文字列を小文字に変換するとは何か、なぜそれをプログラマーは行うのか?

文字列を小文字に変換するとは、文字列内の全ての大文字を対応する小文字に変換する操作を指します。これは、文字列の比較や検索を行う際に大文字と小文字の違いを無視したいときに特に役立つ技術です。

# 実現方法:

Powershellで文字列を小文字に変換するのは驚くほど簡単です。以下にコード例を示します。

```PowerShell
$str = "Hello, World!"

# Convert the string to lowercase.
$lower = $str.ToLower()

# Output the result.
Write-Output $lower
```

このコードは次のような出力を生成します:

```PowerShell
hello, world!
```

# 詳細情報:

**歴史的な文脈**: 文字列の小文字変換は、プログラミングの初期から存在しています。それは、文字列の比較やソート、検索処理が容易になるためです。また、一部の文化では、文章全体を小文字で表示する慣習がありました。

**代替手段**: Powershellでは、'-ieq'等のcase-insensitive演算子を使用して、大文字小文字を無視した比較を行うことも可能です。例えば：

```PowerShell
$str = "Hello, World!"
if ($str -ieq "hello, world!") {
    Write-Output "They match."
}
```

**仕組み**: `.ToLower()`はメソッドで、文字列に対して呼び出されます。これは、文字列内の各大文字文字を対応する小文字に変換します。

# 関連情報:

1. [MSDN String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0): `.ToLower()`メソッドの詳細な説明と使い方について記載があります。
2. [MSDN About_Comparison_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators): Powershellの比較演算子について詳しく説明されています。