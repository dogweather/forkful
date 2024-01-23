---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラーへの書き込みとは、プログラムがエラーメッセージや診断情報を出力するための専用チャネルです。これを用いることで、正常な出力とエラーメッセージを分け、ログやユーザへのフィードバックが改善されます。

## How to:
エラーメッセージを標準エラーに出力する例:

```Bash
echo "エラー: ファイルが見つかりません。" 1>&2
```

このコードは以下のような出力となります:
```
エラー: ファイルが見つかりません。
```

## Deep Dive
標準エラーはUNIXとその派生システムで利用できる3つの主要なストリームの一つです。標準出力（stdout）と標準入力（stdin）と共に、利用者がプログラムの挙動を柔軟に扱えるようにします。代替として、シェルスクリプトでは`>&`演算子を用いてエラーメッセージをファイルにリダイレクトすることもできます。シェル内部では、ファイルディスクリプタ2が標準エラーに関連づけられています。

## See Also
- GNU Bash documentation: https://www.gnu.org/software/bash/manual/
- The Linux Command Line by William Shotts: https://linuxcommand.org/tlcl.php
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
