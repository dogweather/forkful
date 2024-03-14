---
date: 2024-01-20 17:56:44.494284-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u30D7\u30ED\u30B0\u30E9\u30E0\u8D77\
  \u52D5\u6642\u306B\u6307\u5B9A\u3059\u308B\u30AA\u30D7\u30B7\u30E7\u30F3\u3084\u30C7\
  \u30FC\u30BF\u306E\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\
  \u67D4\u8EDF\u6027\u3092\u6301\u305F\u305B\u3001\u540C\u3058\u30B3\u30FC\u30C9\u3067\
  \u7570\u306A\u308B\u6319\u52D5\u3092\u3055\u305B\u308B\u305F\u3081\u3001\u3053\u306E\
  \u6280\u8853\u306F\u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.519206-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u30D7\u30ED\u30B0\u30E9\u30E0\u8D77\
  \u52D5\u6642\u306B\u6307\u5B9A\u3059\u308B\u30AA\u30D7\u30B7\u30E7\u30F3\u3084\u30C7\
  \u30FC\u30BF\u306E\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\
  \u67D4\u8EDF\u6027\u3092\u6301\u305F\u305B\u3001\u540C\u3058\u30B3\u30FC\u30C9\u3067\
  \u7570\u306A\u308B\u6319\u52D5\u3092\u3055\u305B\u308B\u305F\u3081\u3001\u3053\u306E\
  \u6280\u8853\u306F\u4F7F\u308F\u308C\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why?
コマンドライン引数を読むとは、ユーザーがプログラム起動時に指定するオプションやデータのことです。プログラムに柔軟性を持たせ、同じコードで異なる挙動をさせるため、この技術は使われます。

## How to:
```Python
import sys

# コマンドライン引数を表示する簡単な例
if __name__ == '__main__':
    print(f'引数の数: {len(sys.argv)}')
    print(f'引数のリスト: {str(sys.argv)}')

    if len(sys.argv) > 1:
        print(f'引数1: {sys.argv[1]}')
```

これを`example.py`として保存し、ターミナルで`python example.py arg1`を実行すると、以下の出力が得られます。

```
引数の数: 2
引数のリスト: ['example.py', 'arg1']
引数1: arg1
```

## Deep Dive:
コマンドライン引数を読むやり方は古くからあります。Unixベースのシステムとそのツールやスクリプトが広く使われた70年代から、コマンドライン引数は使われてきました。Pythonでは`sys`モジュールがよく使用されますが、より高機能な`argparse`ライブラリもあります。

`sys.argv`はシンプルで使いやすいが、複雑な引数処理には向いていません。`argparse`は引数をパースし、ヘルプメッセージを自動生成し、エラーハンドリングも容易にします。さらに、引数をより明示的に定義でき、デフォルト値や型チェックもサポートしています。

例外処理や引数の検証を自力で行わなければならない`sys.argv`に比べ、`argparse`は強力で、プログラムの使い方がいっそう明確になります。

## See Also:
- Python `sys`モジュールのドキュメント: https://docs.python.org/3/library/sys.html
- Python `argparse`モジュールのドキュメント: https://docs.python.org/3/library/argparse.html
