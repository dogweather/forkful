---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から特定のパターンに一致する文字を削除することは、不要な文字や特定の形式に適合しない文字を取り除くための一般的な走査です。これは、データの統一性を確保し、あるいはデータの解析や操作を容易にするために、プログラマがしばしば行う作業です。

## 使い方

以下に例を示します：

```PowerShell
# 文字列定義
$str = "b2b4b7"

# パターン一致する文字を削除
$pattern = "[0-9]"
$str -replace $pattern, ""
```

実行結果：

```PowerShell
bbb
```

数字'2','4','7'が削除され、結果として'b2b4b7'が'bbb'に変換されます。

## 深掘り

歴史的なコンテキストでは、パターンに一致する文字列の削除はテキストデータの解析や操作で常に有用であり、それは古くからプログラミングの要素の一つでした。PowerShellでは、-replace演算子を使ってこのタスクを達成します。他の方法としては、[regex]::replaceメソッド挙げられます。

実装の詳細については、-replace演算子は、パターンに一致する全てのインスタンスを削除します。また、この操作は大文字と小文字を区別します。

## 参考資料

詳しくは以下を参照してください：

- PowerShellの置換演算子について：https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_comparison_operators
- 正規表現の使用方法について：https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference