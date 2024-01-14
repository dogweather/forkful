---
title:                "TypeScript: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することの重要性、そしてその方法を紹介します。

## ハウツー

ディレクトリが存在するかどうかを確認するには、`fs.existSync`メソッドを使用します。以下のコードを参考にしてください。

```TypeScript
import * as fs from 'fs';

const directoryPath = 'my-directory';

// ディレクトリが存在するか確認
if (fs.existsSync(directoryPath)) {
  console.log('ディレクトリが存在します。');
} else {
  console.log('ディレクトリは存在しません。');
}
```

上記のコードを実行すると、`my-directory`ディレクトリが存在する場合は`ディレクトリが存在します。`というメッセージが表示されます。もしディレクトリが存在しない場合は、`ディレクトリは存在しません。`というメッセージが表示されます。

## ディープダイブ

`fs.existSync`メソッドを使用すると、同期的にディレクトリの存在を確認することができます。これは非同期的に確認する`fs.exists`メソッドと比べて、コードがシンプルになります。ただし、大きなファイルシステムをスキャンする場合には、パフォーマンスの低下が起こる可能性があります。そのため、大きなファイルシステムをスキャンする場合には、非同期的な方法でディレクトリの存在を確認することが推奨されます。

## See Also

- [fs Module - Node.js Docs](https://nodejs.org/api/fs.html)
- [Checking if a file or directory exists using Node.js - Stack Overflow](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)