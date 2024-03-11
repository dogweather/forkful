---
date: 2024-01-26 03:42:01.385548-07:00
description: "PowerShell\u3067\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\
  \u3092\u524A\u9664\u3059\u308B\u3068\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u5468\u308A\
  \u3092\u56F2\u3093\u3067\u3044\u308B\u5358\u4E00\uFF08`'`\uFF09\u307E\u305F\u306F\
  \u4E8C\u91CD\uFF08`\"`\uFF09\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\u304D\u307E\
  \u3059\u3002\u7279\u306B\u30E6\u30FC\u30B6\u30FC\u306E\u5165\u529B\u3084\u30D5\u30A1\
  \u30A4\u30EB\u306E\u30D1\u30FC\u30B9\u3092\u6271\u3046\u5834\u5408\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u51E6\u7406\u3001\u6BD4\u8F03\u3001\u307E\u305F\u306F\
  \u51FA\u529B\u306E\u76EE\u7684\u3067\u6587\u5B57\u5217\u3092\u30AF\u30EA\u30FC\u30F3\
  \u30A2\u30C3\u30D7\u3059\u308B\u5FC5\u8981\u304C\u3088\u304F\u3042\u308A\u307E\u3059\
  \u3002"
lastmod: '2024-03-11T00:14:15.968991-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\u3092\
  \u524A\u9664\u3059\u308B\u3068\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u5468\u308A\u3092\
  \u56F2\u3093\u3067\u3044\u308B\u5358\u4E00\uFF08`'`\uFF09\u307E\u305F\u306F\u4E8C\
  \u91CD\uFF08`\"`\uFF09\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\u304D\u307E\u3059\
  \u3002\u7279\u306B\u30E6\u30FC\u30B6\u30FC\u306E\u5165\u529B\u3084\u30D5\u30A1\u30A4\
  \u30EB\u306E\u30D1\u30FC\u30B9\u3092\u6271\u3046\u5834\u5408\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u51E6\u7406\u3001\u6BD4\u8F03\u3001\u307E\u305F\u306F\u51FA\
  \u529B\u306E\u76EE\u7684\u3067\u6587\u5B57\u5217\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\
  \u30C3\u30D7\u3059\u308B\u5FC5\u8981\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
---

{{< edit_this_page >}}

## はじめに：何となく理由
PowerShellで文字列からクォートを削除すると、テキストの周りを囲んでいる単一（`'`）または二重（`"`）引用符を取り除きます。特にユーザーの入力やファイルのパースを扱う場合、プログラマーは処理、比較、または出力の目的で文字列をクリーンアップする必要がよくあります。

## 方法：
文字列からクォートを削除するために、`-replace` 演算子を使用できます。以下の方法で行います：

```PowerShell
# 単一引用符を置換
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # 出力：Hello, World!

# 二重引用符を置換
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # 出力：Hello, World!
```

両方のタイプに対して：

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # 正規表現キャラクタークラスの使用に注目
Write-Output $cleanString  # 出力：Hi there, she said.
```

コンソールからのサンプル出力は以下のようになります：

```
Hello, World!
Hello, World!
Hi there, she said.
```

## 詳細情報
以前は、PowerShellがMicrosoftの計画の一部でさえなかった時代に、Windowsでのテキスト処理はしばしば機能に限りがあるバッチスクリプトの範疇でした。PowerShellの導入により、強力な文字列操作機能がもたらされ、スクリプト作成がはるかに堅牢なものとなりました。

`-replace` に代わる方法も存在しますが、例えば `.Trim()` メソッドを使用して文字列の開始と終了のクォートのみを削除することができますが、同じ制御や正規表現のサポートを提供するわけではありません。

```PowerShell
# 開始と終了のクォートに対して .Trim() を使用
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # 出力：Hello, World!
```

注記：`-replace`は裏で正規表現を使用しているので、それを扱う際には、対象とする特別な文字がある場合はエスケープする必要があることに注意してください。より詳細なクォートの削除制御が必要な場合、`-replace` を使って正規表現に深く潜ることで、大幅な柔軟性を得ることができます。

## 関連情報
- PowerShellでの正規表現についての詳細は、公式ドキュメントを参照してください：[about_Regular_Expressions](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- 他の文字列メソッドを発見する：[Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.trim?view=net-6.0)
