---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:33.475880-07:00
description: "\u65B9\u6CD5: Haskell\u306F\u305D\u306E\u57FA\u672C\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u901A\u3058\u3066\u3001\u4E3B\u306B`System.Directory`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\
  \u306E\u5B58\u5728\u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u76F4\u63A5\u7684\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u57FA\u672C\u7684\
  \u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046."
lastmod: '2024-03-13T22:44:42.205609-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u306F\u305D\u306E\u57FA\u672C\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u901A\u3058\u3066\u3001\u4E3B\u306B`System.Directory`\u30E2\u30B8\u30E5\u30FC\u30EB\
  \u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\
  \u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\
  \u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法:
Haskellはその基本ライブラリを通じて、主に`System.Directory`モジュールを使用してディレクトリの存在をチェックする直接的な方法を提供しています。基本的な例を見てみましょう:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "ディレクトリは存在しますか? " ++ show exists
```

サンプル出力は、ディレクトリが存在するか否かによって異なります:

```
ディレクトリは存在しますか? True
```
または:
```
ディレクトリは存在しますか? False
```

より複雑なシナリオや追加機能については、ファイルパスをより抽象的に扱うために`filepath`のような人気のあるサードパーティ製ライブラリを検討するかもしれません。しかし、単にディレクトリが存在するかどうかを確認する目的では、基本ライブラリの`System.Directory`で十分で効率的です。

ファイルシステムの扱いはプラットフォームによって異なる場合があることを覚えておいてください。Haskellのアプローチはこれらの違いの一部を抽象化しようとします。ターゲットシステムでファイル操作を常にテストして、期待される動作を確認してください。
