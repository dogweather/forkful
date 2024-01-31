---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
simple_title:         "標準エラーへの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラーって何？なんで使うの？

プログラムは出力を標準出力と標準エラーに書き分ける。標準エラーはエラーメッセージや警告に使い、ログと実際の出力を分けるために便利。

## How to:
コード例とサンプル出力

```Swift
import Foundation

// 標準出力への書き込み
print("This is a normal message.")

// 標準エラーへの書き込み
fputs("This is an error message.\n", stderr)
```

実行結果:
```
This is a normal message.
This is an error message.
```

## Deep Dive
深堀り情報

- 歴史的背景: Unix由来で、プログラムは2つの主要な出力ストリームを持つ。
- 代替案: ファイルやデータベースへのロギングでもエラーを記録可能。
- 実装の詳細: `fputs`はC言語標準関数で、Swiftでも使用可能。`stderr`はグローバル変数で標準エラーストリームを指す。

## See Also
関連情報へのリンク

- Swiftの公式ドキュメント: [Swift.org](https://www.swift.org/)
- Unixの標準ストリームについての詳細: [Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)
- Appleのデバッグガイド: [Logging](https://developer.apple.com/documentation/os/logging)
