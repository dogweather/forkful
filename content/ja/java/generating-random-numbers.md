---
title:                "ランダムな数を生成する"
html_title:           "Java: ランダムな数を生成する"
simple_title:         "ランダムな数を生成する"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何かしら？ 
ランダムな数字の生成とは、偶然の結果を発生させるプログラミング技術です。プログラマーがこれをする理由は、例えばランダムなゲームやプログラムを作成するためです。

## 方法：
ランダムな数字を生成するには、Javaの標準ライブラリである`Random`クラスを使用します。`Random`クラスのインスタンスを作成し、`nextInt()`メソッドを使用することで、指定した範囲内でランダムな数字を生成することができます。例えば、以下のコードでは1から10までのランダムな数を生成しています。

```Java
Random random = new Random();
int randomNumber = random.nextInt(10) + 1;
System.out.println(randomNumber); // 1から10までのランダムな数が出力される
```

## 深堀り：
ランダムな数字の生成は、数学的にもコンピューター科学的にも興味深いトピックです。ランダムなアルゴリズムの開発には長い歴史があり、現在でもさまざまなアルゴリズムが使用されています。また、ランダムな数字を生成する他の方法としては、ハードウェアデバイスを使用する方法もあります。しかし、Javaの`Random`クラスは予測不能で十分ランダムな数字を生成することができます。

## 関連リンク：
- [Javaでランダムな数字を生成する方法](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [より高度なランダムな数字の生成アルゴリズムについての情報](https://www.random.org/randomness/)
- [ハードウェアデバイスを使用した本当にランダムな数字の生成について](https://www.random.org/analysis/)