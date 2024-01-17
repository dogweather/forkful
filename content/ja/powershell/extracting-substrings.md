---
title:                "「サブストリングの抽出」"
html_title:           "PowerShell: 「サブストリングの抽出」"
simple_title:         "「サブストリングの抽出」"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何をして、なぜ？　
部分文字列を抽出するとは何かを説明し、プログラマーがそれをする理由を2〜3文で説明します。

部分文字列を抽出するとは、文字列の一部を取り出すことです。例えば、 "Hello World"という文字列から"World"という部分文字列を抽出することができます。プログラマーが部分文字列を抽出する理由は、文字列を処理する際に、必要な情報のみを取り出して使用するためです。

## 方法：
下の「PowerShell ...」コードブロック内のコーディング例とサンプルの出力をご覧ください。

```PowerShell
# 文字列から部分文字列を取得する例
$string = "Hello World"
$substring = $string.Substring(6,5) # 開始位置6から5文字分取得する
Write-Output $substring # 出力結果は"World"となります
```

```PowerShell
# 文字列を文字ごとに区切る例
$string = "PowerShell"
$characters = $string.ToCharArray() # 文字配列に変換する
Write-Output $characters # 出力結果は"P", "o", "w", "e", "r", "S", "h", "e", "l", "l"となります
```

## 詳細説明：
部分文字列を抽出する方法には、いくつかのアプローチがあります。以前は、C言語の関数である「substr」を使用する方法が一般的でしたが、現在は多くのプログラミング言語で部分文字列を扱うための組み込み機能が備わっています。

他のプログラミング言語ではどのように部分文字列を扱っているかについては、関連ソースをご参照ください。

## 関連リンク：
- [PowerShellで文字列を扱う方法 (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_string)](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_string)
- [C言語の「substr」関数 (https://www.tutorialspoint.com/c_standard_library/c_function_substr.htm)](https://www.tutorialspoint.com/c_standard_library/c_function_substr.htm)