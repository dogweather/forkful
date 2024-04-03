---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:29.331426-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Python\u306F`os`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3068`pathlib`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\
  \u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\
  \u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306E\u30CD\u30A4\
  \u30C6\u30A3\u30D6\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u3053\u3053\u3067\u306F\u3001\u4E21\u65B9\u306E\u4F8B\u3092\u7D39\u4ECB\u3057\
  \u307E\u3059\uFF1A #."
lastmod: '2024-03-13T22:44:41.518017-06:00'
model: gpt-4-0125-preview
summary: "Python\u306F`os`\u30E2\u30B8\u30E5\u30FC\u30EB\u3068`pathlib`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\
  \u304C\u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\
  \u305F\u3081\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u4E21\u65B9\u306E\
  \u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3059\uFF1A\n\n#."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
