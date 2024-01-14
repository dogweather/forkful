---
title:                "Python: 「標準エラーへの書き込み」"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする上で、エラーメッセージは非常に重要なものです。エラーメッセージが適切に表示されない場合、問題のトラッキングや修正が難しくなります。そのため、Pythonでは標準エラーに対して直接書き込むことができるようになっています。

## 方法

まずは標準エラーを使用するために、sysモジュールをインポートします。

```Python
import sys
```

次に、print関数の代わりに、sysモジュールのstderrオブジェクトを使用してエラーメッセージを出力します。

```Python
sys.stderr.write("エラーが発生しました。")
```

もしくは、フォーマット済みのエラーメッセージを出力することもできます。

```Python
error_message = "不正な入力です。"
sys.stderr.write("エラー： {}".format(error_message))
```

このように、標準エラーを使用することで、より詳細なエラーメッセージを出力することができます。

## 深く掘り下げる

標準エラーは、標準出力と同じくファイルオブジェクトです。そのため、標準エラーに対してもwrite()やwritelines()メソッドを使用することができます。また、sysモジュールのseterr()関数を使用することで、標準エラーの挙動をカスタマイズすることもできます。

## 参考リンク

- [Python公式ドキュメント - sysモジュール](https://docs.python.org/ja/3/library/sys.html)
- [Python公式ドキュメント - 標準エラーと標準出力](https://docs.python.org/ja/3/tutorial/inputoutput.html#standard-error-and-standard-output)
- [Yamaronのブログ - 標準出力と標準エラーの使い分け](https://blog.yamaron.net/entry/2015/02/10/210000)