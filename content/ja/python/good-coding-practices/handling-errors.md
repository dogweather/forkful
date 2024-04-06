---
date: 2024-01-26 00:56:32.413531-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\
  \u30B0\u304C\u59CB\u307E\u3063\u3066\u4EE5\u6765\u3001\u30A8\u30E9\u30FC\u51E6\u7406\
  \u306F\u91CD\u8981\u3067\u3057\u305F\u3002\u521D\u671F\u306E\u30A2\u30D7\u30ED\u30FC\
  \u30C1\u306F\u57FA\u672C\u7684\u306A\u3082\u306E\u3067\u3001\u30EA\u30B9\u30AF\u306E\
  \u3042\u308B\u64CD\u4F5C\u306E\u524D\u306B\u6761\u4EF6\u3092\u30C1\u30A7\u30C3\u30AF\
  \u3059\u308B\u3088\u3046\u306B\u3057\u3066\u3044\u307E\u3057\u305F\u3002Python\u306E\
  `try-except`\u69CB\u6587\u306F\u3001C++\u3084Java\u306A\u3069\u306E\u53E4\u3044\u8A00\
  \u8A9E\u3067\u306E\u4F8B\u5916\u51E6\u7406\u306E\u907A\u7523\u304B\u3089\u6765\u3066\
  \u304A\u308A\u3001\u30D7\u30ED\u30BB\u30B9\u3092\u5358\u7D14\u5316\u3057\u3066\u3044\
  \u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:50:55.512236-06:00'
model: gpt-4-1106-preview
summary: "`try`\u30D6\u30ED\u30C3\u30AF\u306E\u30B3\u30FC\u30C9\u3092\u8A66\u3059\u3068\
  \u304D\u3001Python\u306F\u3044\u304B\u306A\u308B\u4F8B\u5916\u3082\u76E3\u8996\u3057\
  \u307E\u3059\u3002\u3082\u3057\u30A8\u30E9\u30FC\u304C\u51FA\u73FE\u3057\u305F\u3089\
  \u3001`except`\u30D6\u30ED\u30C3\u30AF\u304C\u305D\u308C\u3092\u6355\u6349\u3057\
  \u307E\u3059\u3002\u6355\u6349\u3059\u308B\u4F8B\u5916\u3092\u5177\u4F53\u7684\u306B\
  \u3057\u305F\u308A\u3001\u4E00\u822C\u7684\u306A`except`\u3067\u3059\u3079\u3066\
  \u3092\u6355\u6349\u3057\u305F\u308A\u3067\u304D\u307E\u3059\u3002\u3057\u304B\u3057\
  \u3001\u5177\u4F53\u7684\u306B\u3059\u308B\u65B9\u304C\u3088\u308A\u826F\u3044\u30A2\
  \u30D7\u30ED\u30FC\u30C1\u3067\u3059\u3002\u305D\u308C\u306F\u7CBE\u78BA\u3067\u3001\
  \u305F\u3060\u306E\u5168\u3066\u3092\u30AD\u30E3\u30C3\u30C1\u3059\u308B\u7DB2\u3067\
  \u306F\u3042\u308A\u307E\u305B\u3093\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

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
