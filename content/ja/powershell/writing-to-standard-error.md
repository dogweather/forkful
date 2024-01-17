---
title:                "標準エラーへの書き込み"
html_title:           "PowerShell: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

何をするために、なぜ書き込むのか？

標準エラーに書き込むことは、プログラマーがエラーメッセージやデバッグ情報をコンソールに表示するための手段です。プログラムが予期せずエラーを発生させたとき、標準エラーにメッセージを書き込むことでプログラマーはそのエラーを修正するための手掛かりを得ることができます。

私たちプログラマーが標準エラーに書き込む理由は、ユーザーにわかりやすくエラーを通知するためです。コンソールに出力されたエラーメッセージを見ることで、ユーザーは何が原因でプログラムが正しく動作しなかったのかを理解することができます。また、標準エラーに書き込まれた情報をもとにプログラムを改善することもできます。

コード例:

```PowerShell
# 標準エラーにメッセージを書き込む
$ErrorMessage = "エラーが発生しました。"
Write-Error $ErrorMessage

# 標準エラー出力を別のファイルにリダイレクトする
Write-Error "エラーが発生しました。" 2>> C:\errors.txt

# 標準入力へのデータの書き込み
Write-Error "エラーが発生しました。" >> $Input
```

出力例:

```PowerShell
エラーが発生しました。

C:\errors.txtファイルにエラーが書き込まれました。

標準入力へのデータが書き込まれました。
```

深層解説:

標準エラーへの書き込みは、標準出力への書き込みとは異なるものです。標準出力はユーザーに表示される情報を含んでいますが、標準エラーはエラーメッセージやデバッグ情報を伝えるために使用されます。このように、プログラムが正しく動作しているときには標準エラーへの書き込みは起こりませんが、予期せずエラーが発生したときには標準エラーにメッセージが書き込まれます。

標準エラーへの書き込みは他の方法でも実現することができます。例えば、標準エラーにメッセージを書き込む代わりに、ログファイルにエラーを記録することもできます。しかし、標準エラーにメッセージを書き込むことが最も一般的な方法です。なぜなら、標準エラーに書き込まれた情報が他の方法よりもすばやくアクセスできるからです。

また、標準エラーに書き込まれたメッセージは通常、赤色のフォントで表示されることで見やすくなります。これにより、エラーメッセージが標準出力のメッセージと混同されることを防ぐことができます。

参考情報:

- [PowerShellの標準エラーについて](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_redirection?view=powershell-7.1#stream-numbers)
- [標準エラー出力をログファイルに書き込む方法](https://www.petri.com/powershell-tip-redirect-errors-log-file)