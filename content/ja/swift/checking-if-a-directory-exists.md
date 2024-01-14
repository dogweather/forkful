---
title:    "Swift: ディレクトリが存在するかどうかをチェックする"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認することのメリットは、プログラムの実行中に必要なファイルやデータがどこにあるかを把握することができることです。

## 使い方
ディレクトリが存在するかどうかを確認するには、`FileManager`クラスの`fileExists(atPath:)`を使用します。下記の例を参考にしてください。

```Swift
if FileManager.default.fileExists(atPath: "path/to/directory") {
    print("ディレクトリが存在します。")
} else {
    print("ディレクトリが存在しません。")
}
```

実行結果は下記のようになります。

```
ディレクトリが存在します。
```

## 詳細
`fileExists(atPath:)`は、指定されたパスがディレクトリかどうかを判断し、存在している場合は`true`、存在しない場合は`false`を返します。`FileManager`クラスには、ファイル操作に便利なメソッドが多数用意されているので、是非チェックしてみてください。

## 参考リンク
- [Swift公式ドキュメント - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [ライブラリー - DirectoryExists](https://github.com/evgenyneu/DirectoryExists)