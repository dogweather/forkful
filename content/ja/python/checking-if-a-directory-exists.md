---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Arduino: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかをチェックすることは、そのディレクトリが実際に存在するかをプログラムに伝えるプロセスです。これは、ディレクトリが必要となる操作を行う前に、エラーを予防するために行います。

## 方法について：

ここでは、Pythonを使ってディレクトリが存在するかどうかを確認する基本的な方法を見てみましょう:

```Python
import os

if os.path.isdir('your_directory'):
    print('The directory exists.')
else:
    print('The directory does not exist.')
```

この例では、`os`モジュールの`isdir` functionを使用しています。`your_directory`が存在する場合、'The directory exists.'と印刷されます。存在しない場合、'The directory does not exist.'と印刷されます。

## ディープダイブ：

ディレクトリの存在確認は計算機のプログラミングの初期から存在しています。Pythonでは、`os.path`モジュールを使ってこれを行います。ただし、Python 3.4からは`pathlib`モジュールも使えます。

`pathlib`を使った例は次のとおりです：

```Python
from pathlib import Path

if Path('your_directory').exists():
    print('The directory exists.')
else:
    print('The directory does not exist.')
```

これらの方法のどちらを使用するかは、Pythonのバージョンとコードの維持性によります。

## 参照：

ディレクトリの存在を確認する他の方法や詳細については、以下のリンクを参照してください：

1. [Official Python documentation on os.path.isdir](https://docs.python.org/3/library/os.path.html#os.path.isdir)
2. [Official Python documentation on pathlib.Path](https://docs.python.org/3/library/pathlib.html#pathlib.Path.exists)
3. [Stack Overflow discussions on checking if a directory exists](https://stackoverflow.com/questions/893323/how-to-find-if-directory-exists-in-python)