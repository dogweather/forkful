---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:58:16.181347-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかどうかをチェックするのは、ファイルシステムに指定したディレクトリが存在するか調べることです。プログラムがエラーなく動作するために、リソースが適切に配置されていることを保証するために必要です。

## How to: (方法)
```Python
import os

# ディレクトリのパスを指定
dir_path = '/path/to/directory'

# ディレクトリが存在するか確認
if os.path.exists(dir_path):
    print(f"存在します: {dir_path}")
else:
    print(f"存在しません: {dir_path}")
```

出力例:
```
存在します: /path/to/directory
```
または
```
存在しません: /path/to/directory
```

## Deep Dive (深堀り)
ディレクトリが存在するかの確認方法は、長い歴史を持ちます。`os.path.exists()` は使いやすいですが、ディレクトリだけでなくファイルも確認できます。`os.path.isdir()` はディレクトリ専用です。Python 3.4 以降では `pathlib` モジュールが導入され、オブジェクト指向スタイルで同じチェックが可能です。

`os` モジュールの実装ディテールは、OS間の違いを抽象化します。WindowsでもUnix系のOSでも、Pythonコードは変わりません。これは開発者にとって非常に便利な特徴です。

代替手段としてシェルコマンドを実行してディレクトリの存在をチェックすることも可能ですが、プラットフォームに依存しやすく、セキュリティリスクも伴うため、推奨されません。

## See Also (参照)
- 公式ドキュメント: [os.path.exists](https://docs.python.org/3/library/os.path.html#os.path.exists)
- 公式ドキュメント: [os.path.isdir](https://docs.python.org/3/library/os.path.html#os.path.isdir)
- 公式ドキュメント: [pathlib.Path.exists](https://docs.python.org/3/library/pathlib.html#pathlib.Path.exists)
