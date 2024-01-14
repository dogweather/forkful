---
title:                "Swift: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成することに興味があるでしょうか？一時ファイルは、データを一時的に保存して、後で必要に応じてアクセスすることができる便利な手段です。例えば、プログラムが実行中に一時的にデータを保存する必要がある場合や、ファイルを永続的に保存する前に一時的にデータを確認したい場合に使われます。

## 作り方

一時ファイルを作成するには、```Swift let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent("temp.txt")``` のように```temporaryDirectory``` プロパティと```appendingPathComponent``` メソッドを使用します。これにより、一時ファイルのディレクトリとファイル名を指定して、一時ファイルのパスを取得することができます。次に、作成したパスを使用して```Swift FileManager.default.createFile(atPath: tempFile.path, contents: nil, attributes: nil)``` のようにファイルを作成します。ここで、```contents```パラメータにはファイルに保存するデータを指定することもできます。最後に、必要な処理が終わったら、```Swift try? FileManager.default.removeItem(at: tempFile)``` を使用してファイルを削除します。

## 詳細を掘り下げる

一時ファイルを作成する際には、一時ファイルのパスを取得する前に、一時フォルダが存在するかどうかをチェックする必要があります。また、作成した一時ファイルを削除する際には、エラー処理を行うことも重要です。さらに、データを一時ファイルに保存する場合は、保存するデータの形式に注意する必要があります。一時ファイルは一時的な保存用であるため、プログラムが終了すると自動的に削除されることが多いため、重要なデータの一時的な保存には適していません。

## See Also

- [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [NSHipster: Temporary Files in Swift](https://nshipster.com/temporary-files/)