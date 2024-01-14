---
title:    "Python: 「文字列を小文字に変換する」"
keywords: ["Python"]
---

{{< edit_this_page >}}

# なぜ

文字列を小文字に変換する必要があるのかを説明します。


コンピュータープログラミングでは、文字列を処理する場合には、文字列を比較したり、検索したり、その他多くの処理を行ったりすることがよくあります。しかし、文字列は大文字と小文字が区別されるため、正しく処理をするためには文字列をすべて同じ形式にする必要があります。

# 方法

Pythonでは、`lower()`メソッドを使用して文字列を小文字に変換することができます。例として、以下のコードを見てみましょう。

``` Python
text = "Hello, WORLD!"
print(text.lower())
```

このコードの出力は`hello, world!`となります。`lower()`は元の文字列を変更せず、小文字に変換した新しい文字列を返します。

また、特定の文字列だけではなく、変数や入力から取得した文字列にも同じように`lower()`を使用することができます。例えば、以下のように入力を受け取って、小文字に変換するプログラムを作ることができます。

``` Python
text = input("文字列を入力してください： ")
print(text.lower())
```

このプログラムは、入力した文字列を小文字に変換して出力するものです。

# 深堀り

Pythonでは`lower()`メソッドだけではなく、`casefold()`メソッドも用意されています。このメソッドは、更に多言語対応しており、文字列の大文字と小文字の区別がない形式に変換してくれます。

また、`lower()`や`casefold()`のようなメソッドが用意されていないプログラミング言語もあります。その場合、自分で文字列を小文字に変換するコードを書く必要があります。一般的には文字のASCIIコードを使って、大文字の文字を小文字に変換しますが、国際化の観点からは十分ではありません。そのため、多言語に対応できるように、より高度なアルゴリズムを使用したり、Unicodeのコードポイントを使用したりする必要があります。

# おすすめの記事

- 文字列を操作する際に役立つPythonの組み込み関数：https://www.python.org/dev/peps/pep-0376/
- Unicodeと文字列の扱い方：https://unicode.org/standard/standard.html

# 関連リンク

- Python公式ドキュメント：https://docs.python.org/ja/3/library/stdtypes.html#str.lower
- Pythonの文字列操作について学ぶ：https://www.tutorialspoint.com/python/python_strings.htm
- Pythonで文字列を扱う上で注意すべきポイント：https://docs.python.org/ja/3/library/string.html