---
date: 2024-01-20 17:56:44.494284-07:00
description: 'How to: .'
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.519206-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
