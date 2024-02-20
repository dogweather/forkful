---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:29.331426-07:00
description: "\u2026"
lastmod: 2024-02-19 22:05:00.791127
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
---

{{< edit_this_page >}}

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
