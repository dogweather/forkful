---
date: 2024-01-20 17:41:31.126940-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) Swift\u3067\u306F\u3001`FileManager`\u3092\
  \u4F7F\u3063\u3066\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u7C21\u5358\u306B\u4F5C\
  \u6210\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.437096-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5\uFF1A) Swift\u3067\u306F\u3001`FileManager`\u3092\u4F7F\u3063\
  \u3066\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u7C21\u5358\u306B\u4F5C\u6210\u3067\
  \u304D\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## How to: (方法：)
Swiftでは、`FileManager`を使って一時ファイルを簡単に作成できます。

```Swift
import Foundation

func createTemporaryFile(prefix: String) -> URL? {
    let temporaryDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
    let fileName = "\(prefix)_\(UUID().uuidString).tmp"
    let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(fileName)
    
    do {
        try "Temporary data".write(to: temporaryFileURL, atomically: true, encoding: .utf8)
        print("Temporary file created: \(temporaryFileURL.path)")
        return temporaryFileURL
    } catch {
        print("Failed to create temporary file: \(error)")
        return nil
    }
}

// Example usage
if let tempFileURL = createTemporaryFile(prefix: "example") {
    // Use the temporary file, then delete it
    try? FileManager.default.removeItem(at: tempFileURL)
}
```

サンプル出力：

```
Temporary file created: /tmp/example_DBA1BC2C-1B77-4F2B-A6F5-426EAF5DDF53.tmp
```

## Deep Dive (詳細解説)
一時ファイルの概念はUNIX系のシステムで遠い昔からあります。`/tmp`ディレクトリは短期間のファイル保管場所として使用されます。Swiftにおける一時ファイルの実装方法は、Foundationフレームワークに由来しています。`FileManager`以外にも、`mkstemp`のような低レベルのC言語APIを使うこともできますが、Swiftでは`FileManager`の方が扱いやすいでしょう。重要なのは、一時ファイルを使い終わったら削除すること、システムの一時ディレクトリを汚さないようにすることです。

## See Also (関連情報)
- Swiftの`FileManager`クラスの公式ドキュメント：
  [FileManager | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
  
- UNIX系の一時ファイルやディレクトリの作成に関するmanページ：
  [mkstemp(3) - Linux man page](https://linux.die.net/man/3/mkstemp)
  
- 安全な一時ファイル作成に関する詳細な議論：
  [Creating temporary files securely](https://www.owasp.org/index.php/Creating_secure_temporary_files)
