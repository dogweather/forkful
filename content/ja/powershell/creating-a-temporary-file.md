---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時ファイルを作成することは、仮のデータを読み書きするための一時的な場所を提供するプロセスです。プログラマーは、大量のデータを操作したり、ファイルをいったん保存して後で復元したりする際に、一時ファイルを使用します。

## 使い方：

PowerShellで一時ファイルを作成する簡単な手順は以下の通りです：

``` PowerShell
# 対話的に一時ファイルを作成する
$tempFile = [IO.Path]::GetTempFileName()
```

これで、`$tempFile`が一時ファイルへのフルパスになります。このコマンドを実行すると、システムの一時フォルダーに一時ファイルが作成されます。

## ディープダイブ：

一時ファイルの概念は、コンピュータが本来のタスクを完了するための拡張「記憶」を必要とするプロセスが考案された、古い集積システムから生まれました。その代替手段として、パイプラインを使用してデータを直接送信する方法がありますが、一時ファイルは中間結果を保持するのに便利です。

PowerShellの`[IO.Path]::GetTempFileName()`メソッドは、一時ファイル名をランダムに生成し、`$env:TEMP`や`$env:TMP`といった環境変数によって指定されたディレクトリに保存します。一時ファイルは、外部資源へのアクセスなしに大量のデータを処理したり、遅延データ読み込みを行ったりするためによく使用されます。

## 関連リンク：

* Microsoftのドキュメンテーション：[GetTempFileName メソッド](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
* PowerShellの環境変数：[`$env:TEMP`や`$env:TMP`](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_environment_variables?view=powershell-7.1)