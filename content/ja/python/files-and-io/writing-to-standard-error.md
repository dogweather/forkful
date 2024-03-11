---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:30.725989-07:00
description: "Python\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\u51FA\u529B\u306F\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08`stdout`\uFF09\u3068\u306F\
  \u5225\u306E\u30A8\u30E9\u30FC\u30B9\u30C8\u30EA\u30FC\u30E0\uFF08`stderr`\uFF09\
  \u306B\u5411\u3051\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\
  \u3001\u6B63\u5E38\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\
  \u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3057\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3084\u30ED\u30B0\u5206\u6790\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.148082-06:00'
model: gpt-4-0125-preview
summary: "Python\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\u51FA\u529B\u306F\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08`stdout`\uFF09\u3068\u306F\
  \u5225\u306E\u30A8\u30E9\u30FC\u30B9\u30C8\u30EA\u30FC\u30E0\uFF08`stderr`\uFF09\
  \u306B\u5411\u3051\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\
  \u3001\u6B63\u5E38\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\
  \u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3057\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3084\u30ED\u30B0\u5206\u6790\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
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
