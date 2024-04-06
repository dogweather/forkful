---
date: 2024-01-27 16:21:16.583429-07:00
description: "\u65B9\u6CD5: \u7C21\u5358\u306A\u30BF\u30B9\u30AF\u304B\u3089\u59CB\
  \u3081\u307E\u3057\u3087\u3046\uFF1Aexample.txt\u3068\u3044\u3046\u30D5\u30A1\u30A4\
  \u30EB\u306E\u3059\u3079\u3066\u306E\"oldtext\"\u3092\"newtext\"\u306B\u7F6E\u63DB\
  \u3057\u305F\u3044\u3068\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\
  \u5B9F\u884C\u3067\u304D\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.431844-06:00'
model: gpt-4-0125-preview
summary: "\u7C21\u5358\u306A\u30BF\u30B9\u30AF\u304B\u3089\u59CB\u3081\u307E\u3057\
  \u3087\u3046\uFF1Aexample.txt\u3068\u3044\u3046\u30D5\u30A1\u30A4\u30EB\u306E\u3059\
  \u3079\u3066\u306E\"oldtext\"\u3092\"newtext\"\u306B\u7F6E\u63DB\u3057\u305F\u3044\
  \u3068\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u5B9F\u884C\u3067\
  \u304D\u307E\u3059\uFF1A."
title: "CLI\u30EF\u30F3\u30E9\u30A4\u30CA\u30FC\u3067\u306E\u30D5\u30A1\u30A4\u30EB\
  \u306E\u30A4\u30F3\u30D7\u30EC\u30FC\u30B9\u7DE8\u96C6"
weight: 32
---

## 方法:


### 単一のファイルでのテキスト置換
簡単なタスクから始めましょう：example.txtというファイルのすべての"oldtext"を"newtext"に置換したいとします。以下の方法で実行できます：

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

このワンライナーは、内容を読み取り、置換を行い、元のファイルに内容を書き戻します。

### 複数のファイルを編集
複数のファイルに同じ変更を適用する必要がある場合はどうでしょうか？ループを使ったアプローチは以下の通りです：

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

このスニペットは、現在のディレクトリのすべての`.txt`ファイルを見つけ、それぞれにおいて"oldtext"を"newtext"に置換します。

### ファイルの始まりまたは終わりに内容を追加
内容を追加または前置することも、簡略化できます：

```PowerShell
# 前置
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# 追加
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

ここでは、新しい内容を既存の内容の前後に連結し、それを保存します。

## ディープダイブ
歴史的に、インプレース編集は`sed`や`awk`のようなUnixツールとより一般的に関連付けられています。より最近の登場者であるPowerShellは、箱から出して直接インプレース編集機能を含まないことが多いです。これは、オブジェクトをテキストストリームより重視するという設計哲学に部分的によるものです。Unixツールはほとんどの入力をテキストとして扱いますが、PowerShellはそうではありません。

このタスクのPowerShell以外の代替手段には、CygwinやWindows Subsystem for Linux（WSL）を通じてWindows上で利用可能な従来のUnixツールの使用が含まれます。これらのツールはテキスト中心の設計により、インプレース編集のためのより簡潔な構文を提供することがよくあります。

実装上では、PowerShellのアプローチは、ファイル全体をメモリに読み込み、変更を行い、それから書き戻すというものであることに注意が必要です。これはモデレートサイズのファイルに対してはうまく機能しますが、非常に大きなファイルの場合には非効率になる可能性があります。そのような場合には、`.NET`メソッドを直接使用するか、大量のデータをストリーミングするように設計された代替ツールの利用を検討するかもしれません。

これらの考慮事項にもかかわらず、PowerShellの柔軟性と広範な機能セットは、Windowsエコシステムに既に浸透している人々やクロスプラットフォーム環境の管理を行っている人々にとって、特にコマンドラインから直接ファイルを操作するための貴重なツールです。
