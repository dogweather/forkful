---
title:                "C#: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックすることの重要性を、わずか1-2文で説明すると、あなたのプログラムがディレクトリが存在しない場合に、予期せぬエラーが発生する可能性があるためです。

## 方法

ディレクトリの存在をチェックするには、C#の "Directory.Exists()" メソッドを使用します。以下の例では、"Documents" という名前のディレクトリが存在するかどうかをチェックしています。

```C#
if(Directory.Exists("Documents"))
{
    Console.WriteLine("Documentsディレクトリが存在します。");
}
else
{
    Console.WriteLine("Documentsディレクトリが存在しません。");
}

// 出力: Documentsディレクトリが存在します。
```

ディレクトリが存在しない場合、"else"ブロックのコードが実行されます。

## ディープダイブ

ディレクトリの存在をチェックする方法は非常にシンプルですが、内部ではどのように動作しているのでしょうか？実際には、"Directory.Exists()" メソッドは "GetFileAttributesW()" Win32 APIを使用しています。このAPIは、指定されたパスに対する属性を取得するために使用されます。ディレクトリの存在を確認するために使用するには、ファイルの属性の値が "FILE_ATTRIBUTE_DIRECTORY" であるかどうかを確認します。

## 関連リンク

- [C#ドキュメント: Directory.Exists(Method)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [Win32 APIドキュメント: GetFileAttributesW(Method)](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesw)