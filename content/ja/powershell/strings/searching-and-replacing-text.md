---
date: 2024-01-20 17:58:26.108661-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) PowerShell \u3067\u306F `Get-Content` \u3068\
  \ `ForEach-Object`, `Replace` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3063\u3066\u30C6\
  \u30AD\u30B9\u30C8\u3092\u63A2\u3057\u3001\u7F6E\u304D\u63DB\u3048\u304C\u3067\u304D\
  \u307E\u3059\u3002\u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.412310-06:00'
model: gpt-4-1106-preview
summary: "PowerShell \u3067\u306F `Get-Content` \u3068 `ForEach-Object`, `Replace`\
  \ \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3063\u3066\u30C6\u30AD\u30B9\u30C8\u3092\
  \u63A2\u3057\u3001\u7F6E\u304D\u63DB\u3048\u304C\u3067\u304D\u307E\u3059\u3002\u7C21\
  \u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
