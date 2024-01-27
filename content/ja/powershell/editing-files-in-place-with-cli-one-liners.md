---
title:                "CLIワンライナーでのファイルのインプレース編集"
date:                  2024-01-27T16:21:16.583429-07:00
model:                 gpt-4-0125-preview
simple_title:         "CLIワンライナーでのファイルのインプレース編集"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## 何となぜ？

PowerShellにおけるCLIワンライナーを使ったファイルのインプレース編集とは、エディタで開くことなく、コマンドラインから直接ファイルを修正することです。このアプローチは時間を節約し、複数のファイルにまたがるバッチ処理や繰り返しの編集タスクの自動化に特に便利です。

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
