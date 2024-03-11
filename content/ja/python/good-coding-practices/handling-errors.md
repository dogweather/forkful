---
date: 2024-01-26 00:56:32.413531-07:00
description: "Python\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\uFF08\u307E\u305F\u306F\
  \u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u3082\uFF09\
  \u3068\u306F\u3001\u4E88\u671F\u305B\u306C\u3053\u3068\u304C\u8D77\u3053\u308B\u3053\
  \u3068\u3092\u60F3\u5B9A\u3057\u3001\u30B3\u30FC\u30C9\u304C\u6B63\u5E38\u3067\u306A\
  \u304F\u306A\u3063\u305F\u3068\u304D\u306B\u4E0A\u624B\u304F\u5BFE\u51E6\u3059\u308B\
  \u6280\u8853\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u30AF\u30E9\u30C3\
  \u30B7\u30E5\u3059\u308B\u306E\u3092\u9632\u304E\u3001\u30E6\u30FC\u30B6\u30FC\u306B\
  \u6307\u793A\u3092\u4E0E\u3048\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u5805\u7262\
  \u304B\u3064\u4FE1\u983C\u6027\u306E\u3042\u308B\u3082\u306E\u306B\u3059\u308B\u305F\
  \u3081\u306B\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.136953-06:00'
model: gpt-4-1106-preview
summary: "Python\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\uFF08\u307E\u305F\u306F\
  \u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u3082\uFF09\
  \u3068\u306F\u3001\u4E88\u671F\u305B\u306C\u3053\u3068\u304C\u8D77\u3053\u308B\u3053\
  \u3068\u3092\u60F3\u5B9A\u3057\u3001\u30B3\u30FC\u30C9\u304C\u6B63\u5E38\u3067\u306A\
  \u304F\u306A\u3063\u305F\u3068\u304D\u306B\u4E0A\u624B\u304F\u5BFE\u51E6\u3059\u308B\
  \u6280\u8853\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u30AF\u30E9\u30C3\
  \u30B7\u30E5\u3059\u308B\u306E\u3092\u9632\u304E\u3001\u30E6\u30FC\u30B6\u30FC\u306B\
  \u6307\u793A\u3092\u4E0E\u3048\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u5805\u7262\
  \u304B\u3064\u4FE1\u983C\u6027\u306E\u3042\u308B\u3082\u306E\u306B\u3059\u308B\u305F\
  \u3081\u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？

Pythonでのエラー処理（または他のプログラミング言語でも）とは、予期せぬことが起こることを想定し、コードが正常でなくなったときに上手く対処する技術です。プログラムがクラッシュするのを防ぎ、ユーザーに指示を与え、プログラムを堅牢かつ信頼性のあるものにするために行います。

## どのように：

``` Python
# 基本的なtry-exceptブロック
try:
    # リスクのあるコード
    number = int(input("Enter a number: "))
except ValueError:
    # エラー処理
    print("That's not a number!")

# 複数の例外を指定
try:
    # 異なる例外を発生させるかもしれないコード
    result = 10 / int(input("Enter a divisor: "))
except ZeroDivisionError:
    print("Oops! Can't divide by zero.")
except ValueError:
    print("I need a number, buddy.")

# elseとfinallyの使用
try:
    number = int(input("Enter a number for squaring: "))
except ValueError:
    print("I said a number!")
else:
    # エラーが発生しなかった
    print("Your number squared is:", number**2)
finally:
    # 常に実行される
    print("Thanks for trying this out!")
```

最初のブロックに無効な数値を入力した場合のサンプル出力:
```
Enter a number: hello
That's not a number!
```

## ディープダイブ

プログラミングが始まって以来、エラー処理は重要でした。初期のアプローチは基本的なもので、リスクのある操作の前に条件をチェックするようにしていました。Pythonの`try-except`構文は、C++やJavaなどの古い言語での例外処理の遺産から来ており、プロセスを単純化しています。

`try`ブロックのコードを試すとき、Pythonはいかなる例外も監視します。もしエラーが出現したら、`except`ブロックがそれを捕捉します。捕捉する例外を具体的にしたり、一般的な`except`ですべてを捕捉したりできます。しかし、具体的にする方がより良いアプローチです。それは精確で、ただの全てをキャッチする網ではありません。

`else`と`finally`はこの概念の追加要素です。`else`ブロックはtryブロックがエラーなしで実行された場合に動作します。`finally`は何があっても実行される信頼できる仲間です。クリーンアップ操作などを考えてください。

他の選択肢はありますか？確かにあります。一部の言語は例外の代わりに戻り値コードを使用します。リソースの管理に`with`ステートメントや、開発中に条件をチェックする`assertions`に遭遇することもあるでしょう。しかし、信頼できるエラー処理戦略について話すとき、try-catchモデルはその可読性と構造で際立っています。

## 参考資料

さらに深く掘り下げるためのいくつかの優れた追加資料です：

- Pythonの公式ドキュメントのエラーと例外について：[Pythonドキュメント – エラーと例外](https://docs.python.org/3/tutorial/errors.html)
- 同主題に関するReal Pythonのガイド：[Real Python - try/except/else/finally ブロック](https://realpython.com/python-exceptions/)
- エラー処理のベストプラクティスに関する考察がなされた議論：[Stack Overflow – 例外を適切に無視する方法は？](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
