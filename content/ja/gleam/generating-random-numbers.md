---
title:                "Gleam: ランダムな数を生成する"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# なぜランダムな数字を生成するのか

プログラミングでよく使われるランダムな数字の生成について説明します。ランダムな数字は、ゲームやアプリケーションの開発でよく使用されるため、知識を身に付けることは非常に重要です。

# 方法

Gleamプログラミング言語を使用してランダムな数字を生成する方法を紹介します。まず、```rand``関数を使用してランダムな数字を生成し、それを変数に代入します。

```Gleam
let random_number = rand()
```

次に、生成されたランダムな数字を特定の範囲内で制限することもできます。例えば、1から10までのランダムな整数を生成するには、```rand_range```関数を使用します。

```Gleam
let random_integer = rand_range(1, 10)
```

ランダムな数字を文字列として扱いたい場合は、```to_string```関数を使用します。

```Gleam
let random_string = to_string(rand())
```

これらの例では、```rand```関数を使用しましたが、Gleamにはさまざまなランダムな数字を生成するための関数が用意されています。詳細については、公式ドキュメントを参照してください。

# ディープダイブ

ランダムな数字の生成には、偏りが生じる可能性があります。そのため、より公平なランダムな数字を生成するためには、より高度なアルゴリズムが必要となります。Gleamでは、これらのアルゴリズムを実装するためのモジュールが用意されています。例えば、```rand/pcg```モジュールや```rand/xoshiro```モジュールなどがあります。

さらに、Gleamではパターンマッチングを使用して、生成されたランダムな数字が特定の条件を満たすかどうかを確認することもできます。

# もっと詳しく

Gleamを使用してランダムな数字を生成する方法やその他の高度な機能については、公式ドキュメントをご覧ください。

# 他にも見てみる

- Gleam公式ドキュメント：https://gleam.run/
- Gleamランダムモジュール：https://docs.gleam.run/stdlib/rand
- さまざまなランダムアルゴリズムの説明：https://en.wikipedia.org/wiki/List_of_random_number_generators