---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ? (What & Why?)

部分文字列の抽出は、既存の文字列から特定の部分を取り出す過程を指します。これはデータのフィルタリング、マッチング、もしくは解析する際にプログラマーによって行われます。

## どうやってやるの? (How to)

以下にPowerShellで部分文字列を抽出する基本的な方法を紹介します。

```PowerShell
# 文字列の宣言
$sourceString = "こんにちは、山田さん"

# 部分文字列の抽出
$substring = $sourceString.Substring(6,4)

# 結果の表示
$substring
```
上記を実行すると、以下のような出力が得られます。

```PowerShell
山田さん
```
この例では、6番目から始まり4文字の部分文字列を抽出しました。

## ディープダイブ (Deep Dive)

PowerShellにおける部分文字列の抽出は、主に .NET Framework のメソッドである `Substring` に依存しています。たとえば、`Substring(6,4)`は6番目の位置から始まって4文字の長さの部分文字列を取り出します。

`Substring`メソッドの代わりに正規表現も利用できます。

```PowerShell
# 正規表現による部分文字列の抽出
$sourceString -match '、(.*?)$'

# 結果の表示
$Matches[1]
```
この方法は、特定のパターンにマッチする部分文字列を抽出するときに非常に便利です。

## 参考資料 (See Also)

-[Microsoft PowerShell 文字列操作の公式ドキュメント](https://docs.microsoft.com/ja-jp/powershell/scripting/learn/deep-dives/everything-about-string-substitutions?view=powershell-7.1)
-[正規表現基本説明](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expressions)

以上、この記事がPowerShellで部分文字列を抽出する際の参考になれば幸いです。