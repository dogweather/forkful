---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "C#: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

ディレクトリが存在するかどうかをチェックするとは、プログラマーが特定のフォルダーがシステム上に存在するかどうかを確認することです。これを行う理由は、ファイルやディレクトリがなければプログラムが正常に動作しないためです。

## 方法：

```C#
if(Directory.Exists(path))
{
    Console.WriteLine("ディレクトリが存在します。");
}
else
{
    Console.WriteLine("ディレクトリが存在しません。");
}
```

上記のコードは、ディレクトリが指定したパス上に存在するかどうかをチェックし、存在する場合には「ディレクトリが存在します。」というメッセージを、存在しない場合には「ディレクトリが存在しません。」というメッセージを表示します。

## 深堀り：

ディレクトリの存在をチェックすることは、古くから一般的なプログラミングの手法の一つです。代替手段として、フォルダー内に特定のファイルが存在するかどうかを確認することもあります。また、この機能を使用するには、System.IO名前空間のDirectoryクラスをインポートする必要があります。

## 関連リンク：

- [MSDNドキュメント（英語）：ディレクトリが存在するかどうかをチェックする方法](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netframework-4.8)
- [プログラミング入門（日本語）：C#でのディレクトリの存在のチェック方法](https://programmingbeginner.net/c-sharp-directory-exists/)