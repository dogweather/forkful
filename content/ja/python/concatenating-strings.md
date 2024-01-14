---
title:    "Python: 文字列の連結"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ: なぜ文字列を連結する必要があるのでしょうか？
Pythonプログラミングでは、文字列を連結することで、より長い文やメッセージを作成することができます。これは、プログラムをより動的にし、より多様な情報を伝えることができるようにするためです。

## 方法：文字列の連結の仕方
文字列を連結するには、文字列を"+"記号でつなげることができます。しかし、より効率的な方法は、Pythonの組み込み関数である`.join()`を使用することです。以下のコードは、3つの文字列を連結する方法を示しています。

```Python
# 文字列を直接連結
print("こんにちは"+"私は"+"Pythonを学んでいます。")

# .join()を使用
my_list = ["Python", "を", "学んでいます。"]
print(" ".join(my_list))

# 出力：
# こんにちは私はPythonを学んでいます。
# こんにちは 私は Pythonを学んでいます。
``` 

## 深堀り：文字列連結の詳細情報
文字列を連結するには、文字列への変更が無いことが重要です。これは、文字列はイミュータブル（変更不可）であるためです。つまり、既存の文字列を変更することはできず、新しい文字列を作成することしかできません。そのため、連結のために新しい文字列を作成する必要があります。

また、`.join()`を使用することで、文字列をより効率的に連結することができます。この方法では、あらかじめリストやタプルなどの反復可能なオブジェクトを作成し、そのオブジェクトの間に連結したい文字列を挿入します。Pythonは、この方法で単一の文字列を作成し、それを出力するため、効率が良いと言えます。

## See Also (参考リンク):
- Pythonの文字列操作: https://docs.python.org/ja/3/library/string.html
- 文字列の連結に関するTutorialspointの記事: https://www.tutorialspoint.com/python/string_join.htm
- Pythonのformattingに関する公式ドキュメント: https://www.python.org/dev/peps/pep-0498/