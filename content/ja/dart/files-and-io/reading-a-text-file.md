---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:33.691091-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.325254-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでテキストファイルを読み込むことは、ファイルシステムに保存されたファイルからデータをアクセスして取得することを含みます。プログラマーはこれを行って、入力データ、設定、またはデータセットを読み込むために、シンプルなスクリプトから複雑なアプリケーションまで多くのアプリケーションで基本的な操作となっています。

## 方法:

Dartのコアライブラリ、`dart:io`は、テキストファイルを同期的または非同期的に読み込むために必要な機能を提供しています。両方のアプローチはこちらです。

**同期的に:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // ファイルを同期的に読み込む
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('ファイル読み取りエラー: $e');
  }
}
```

**非同期的に:**

特に大きなファイルや反応性の高いアプリケーションでファイルが読み込まれている間、プログラムがブロックされないようにするために:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('ファイル読み取りエラー: $e');
  }
}
```

**サンプル出力:**

テキストファイルに以下の内容が含まれている場合:

```
Hello, Dart!
```

上記の両方の方法で出力されます:

```
Hello, Dart!
```

**サードパーティのライブラリを使用する:**

ファイル操作の単純化やエラーハンドリングの強化など、追加の機能を求める場合は、`package:file`のようなサードパーティライブラリを検討するかもしれません。しかし、私の最終更新時点では、上記で示したように、コアの`dart:io`パッケージを直接使用することが、Dartでテキストファイルを読み込むための最も一般的で直接的な方法です。
