---
title:                "標準エラーへの書き込み"
aliases:
- /ja/python/writing-to-standard-error/
date:                  2024-02-03T19:34:30.725989-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Pythonでの標準エラー出力は、プログラムのエラーメッセージや診断を標準出力（`stdout`）とは別のエラーストリーム（`stderr`）に向けることを意味します。プログラマーはこれを行うことで、正常なプログラム出力とエラーメッセージを区別し、デバッグやログ分析を容易にします。

## 方法:
### `sys.stderr`を使用する
Pythonの組み込み`sys`モジュールは`stderr`への明示的な書き込みを許可します。このアプローチは、単純なエラーメッセージや診断に直接的です。

```python
import sys

sys.stderr.write('Error: Something went wrong.\n')
```
標準エラー出力へのサンプル出力：
```
Error: Something went wrong.
```

### `print`関数を使う
Pythonの`print`関数は、`file`パラメータを指定することで出力を`stderr`にリダイレクトできます。この方法は`print`のユーザーフレンドリーさを活用しつつ、エラーメッセージを扱うのに便利です。
```python
from sys import stderr

print('Error: Failure in module.', file=stderr)
```
標準エラー出力へのサンプル出力：
```
Error: Failure in module.
```

### `logging`モジュールを使用する
より包括的な解決策として、Pythonの`logging`モジュールはメッセージを`stderr`に直接送るだけでなく、ファイルへの書き込みやメッセージ形式のカスタマイズなど、さまざまなことを実現できます。この方法は、様々なログレベル、メッセージ形式、または宛先が必要なアプリケーションに最適です。
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: Database connection failed.')
```
標準エラー出力へのサンプル出力：
```
ERROR:__main__:Error: Database connection failed.
```

### サードパーティのライブラリ：`loguru`
`loguru`はPythonアプリケーションのログを簡素化する人気のサードパーティライブラリです。エラーを自動的に`stderr`に向けるなど、さまざまな機能を提供します。

`loguru`を使用するには、まずpip経由でインストールします：
```shell
pip install loguru
```

次に、以下のようにPythonスクリプトに組み込みます：
```python
from loguru import logger

logger.error('Error: Failed to open file.')
```
標準エラー出力へのサンプル出力：
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: Failed to open file.
```
