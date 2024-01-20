---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テストコードを書くことで、プログラムが期待通りに動くことを確認します。バグを早期に発見し、品質を保つためにプログラマーはテストをします。

## How to: (方法)
```Python
# sample.py
def add(a, b):
    return a + b

# test_sample.py
import unittest
from sample import add

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(2, 3), 5)

if __name__ == '__main__':
    unittest.main()
```
サンプル出力:
```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Deep Dive (深掘り)
テストは1970年代に始まり、Extreme Programmingへと進化しました。代替手法にはTDD（テスト駆動開発）があります。Pythonでは`unittest`が標準のテストフレームワークですが、`pytest`や`nose`などのサードパーティ製のツールもあります。また、モックオブジェクトを使用して外部システムとの連携をテスト擬似的に行うこともできます。

## See Also (関連情報)
- Pythonの公式ドキュメント: [unittest — Unit testing framework](https://docs.python.org/3/library/unittest.html)
- pytest: [https://docs.pytest.org/en/stable/](https://docs.pytest.org/en/stable/)
- TDDについて: [テスト駆動開発](https://ja.wikipedia.org/wiki/テスト駆動開発)