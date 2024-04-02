---
date: 2024-01-20 17:58:26.108661-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C6\u30AD\u30B9\u30C8\u3092\
  \u63A2\u3057\u3066\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3001\u30D5\u30A1\
  \u30A4\u30EB\u3084\u30B3\u30FC\u30C9\u5185\u306E\u6587\u5B57\u5217\u3092\u7D20\u65E9\
  \u304F\u5909\u66F4\u3057\u307E\u3059\u3002\u4F5C\u696D\u52B9\u7387\u3092\u4E0A\u3052\
  \u308B\u305F\u3081\u3001\u30A8\u30E9\u30FC\u3092\u4F4E\u6E1B\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u306F\u91CD\u8981\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.412310-06:00'
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C6\u30AD\u30B9\u30C8\u3092\
  \u63A2\u3057\u3066\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3001\u30D5\u30A1\
  \u30A4\u30EB\u3084\u30B3\u30FC\u30C9\u5185\u306E\u6587\u5B57\u5217\u3092\u7D20\u65E9\
  \u304F\u5909\u66F4\u3057\u307E\u3059\u3002\u4F5C\u696D\u52B9\u7387\u3092\u4E0A\u3052\
  \u308B\u305F\u3081\u3001\u30A8\u30E9\u30FC\u3092\u4F4E\u6E1B\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u306F\u91CD\u8981\u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## What & Why? (何となぜ？)
プログラマーはテキストを探して置き換えることで、ファイルやコード内の文字列を素早く変更します。作業効率を上げるため、エラーを低減するためにこれは重要です。

## How to: (方法：)
PowerShell では `Get-Content` と `ForEach-Object`, `Replace` メソッドを使ってテキストを探し、置き換えができます。簡単な例を見てみましょう。

```PowerShell
# ファイルからテキストを読み込む
$content = Get-Content 'example.txt'

# 'oldtext' を 'newtext' に置き換える
$content = $content -replace 'oldtext', 'newtext'

# 結果をファイルに出力する
$content | Set-Content 'example.txt'
```

置き換えた後のファイル内容を出力してみます:

```PowerShell
Get-Content 'example.txt'
```

これが出力です:

```
newtext and some other text.
```

## Deep Dive (深掘り：)
歴史的に、テキストの検索置き換えは編集作業を自動化するために使われてきました。`sed`や`awk`のようなユニックスツールも同様のタスクに使いますが、PowerShellはWindows環境における豊かな機能性とスクリプトの統合性を提供しています。

PowerShell では `-replace` 演算子の他に `.Replace()` メソッドや `Select-String` コマンドレットを使う方法もあります。`-replace` は正規表現をサポートし、`.Replace()` は単純な文字列置換にとどまります。

## See Also (関連情報：)
- [Microsoftの公式ドキュメント: about_Comparison_Operators](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_Comparison_Operators?view=powershell-7.2)
- [Microsoftの公式ドキュメント: Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.2)
- [Microsoftの公式ドキュメント: Set-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/set-content?view=powershell-7.2)
- [Stack Overflow: PowerShellでのテキストの検索と置換](https://stackoverflow.com/questions/tagged/powershell+replace)
