---
title:    "Python: 文字列の連結"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

文字列を連結することによって、プログラムでより複雑な文を作成することができます。例えば、名前のリストにあるすべての名前を表示する場合、文字列を連結することで簡単に行うことができます。

## 使い方

```Python
# 2つの文字列を連結する例
first_name = "太郎"
last_name = "山田"
full_name = first_name + last_name
print(full_name)
```

出力結果: 太郎山田

```Python
# 文字列と数値を連結する例
message = "今日は"
temperature = 25
full_message = message + str(temperature) + "度です"
print(full_message)
```

出力結果: 今日は25度です

## 深堀り

文字列の連結には、「+」演算子か「str.join()」メソッドを使用することができます。ただし、文字列の連結を繰り返し行う場合は、リスト内包表記を使用した方が効率的です。

また、文字列の連結は不可変オブジェクトであるため、連結のたびに新しいオブジェクトが作成されることに注意が必要です。長い文字列を連結する場合は、マルチライン文字列を使用することでコードの見やすさを向上させることができます。

## 参考

[Pythonドキュメント - 文字列のメソッド](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)

[Real Python - Pythonで文字列を連結する方法](https://realpython.com/python-strings/)

See Also

[マークダウンの基本シンタックス](https://www.markdownguide.org/basic-syntax/)

[Pythonドキュメント - マルチライン文字列](https://docs.python.org/ja/3/graphics.html#graphics)