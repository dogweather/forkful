---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:29.331426-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.518017-06:00'
model: gpt-4-0125-preview
summary: "Python\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\
  \u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u66F8\u304D\u3059\u308B\u3088\u3046\u306A\
  \u64CD\u4F5C\u3092\u884C\u3046\u524D\u306B\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\
  \u30C6\u30E0\u5185\u306E\u30D5\u30A9\u30EB\u30C0\u30FC\u306E\u5B58\u5728\u3092\u78BA\
  \u8A8D\u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001`FileNotFoundError`\u306E\u3088\u3046\u306A\u30A8\
  \u30E9\u30FC\u3092\u907F\u3051\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u3068\u5BFE\
  \u8A71\u3057\u3088\u3046\u3068\u3057\u305F\u6642\u306B\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u304C\u30AF\u30E9\u30C3\u30B7\u30E5\u3057\u306A\u3044\u3088\u3046\
  \u306B\u3001\u78BA\u5B9F\u306B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304C\
  \u4FE1\u983C\u6027\u3092\u6301\u3063\u3066\u52D5\u4F5C\u3059\u308B\u3088\u3046\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## なぜ & どうして？
Pythonでディレクトリが存在するかどうかを確認することは、ファイルを読み書きするような操作を行う前に、ファイルシステム内のフォルダーの存在を確認することについてです。プログラマーは、`FileNotFoundError`のようなエラーを避け、ディレクトリと対話しようとした時にアプリケーションがクラッシュしないように、確実にアプリケーションが信頼性を持って動作するようにこれを行います。

## どのようにして：
Pythonは`os`モジュールと`pathlib`モジュールを使用してディレクトリが存在するかどうかを確認するためのネイティブな方法を提供しています。ここでは、両方の例を紹介します：

### `os` モジュールを使用する場合
```python
import os

# ディレクトリのパスを指定
dir_path = "/path/to/directory"

# ディレクトリが存在するか確認
if os.path.isdir(dir_path):
    print(f"ディレクトリ {dir_path} は存在します。")
else:
    print(f"ディレクトリ {dir_path} は存在しません。")
```

### `pathlib` モジュールを使用する場合
```python
from pathlib import Path

# ディレクトリのパスを指定
dir_path = Path("/path/to/directory")

# ディレクトリが存在するか確認
if dir_path.is_dir():
    print(f"ディレクトリ {dir_path} は存在します。")
else:
    print(f"ディレクトリ {dir_path} は存在しません。")
```

### サードパーティのライブラリ
Pythonの標準ライブラリだけでディレクトリが存在するかどうかを確認するのに十分ですが、`pathlib2`のようなライブラリは、Pythonのバージョンによる一貫性や追加の機能のための代替案となることがあります。

***注：*** 最新のPythonバージョンでは、ほとんどの用途に対して`pathlib`が十分に強力であるため、この特定のタスクにサードパーティのライブラリが必要となることは少なくなっています。
