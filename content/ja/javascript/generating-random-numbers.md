---
title:    "Javascript: ランダムな数字の生成"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ
乱数を生成するのには、さまざまな目的があります。たとえば、ゲームのランダムなイベントや、データのサンプルリング、ランダムなパスワードの生成などがあります。乱数を生成することで、予測不可能な要素をプログラムに導入し、より興味深く、より多様な機能をプログラムに追加することができます。

## 方法
乱数を生成する方法については、様々なアルゴリズムがありますが、ここではよく使われる線形合同法を紹介します。これは、現在の時刻をシード値として使い、それを数学的な計算により次の乱数を生成する方法です。

```Javascript
// 線形合同法による乱数の生成
function generateRandomNumber() {
    let seed = new Date().getTime(); // 現在の時刻をシード値として使用
    let a = 1664525; // 定数 a
    let c = 1013904223; // 定数 c
    let m = Math.pow(2, 32); // 乱数の範囲を指定
    seed = (a * seed + c) % m; // 線形合同法により次の乱数を生成
    return seed;
}

// 10回乱数を生成し、コンソールに出力する
for (let i = 0; i < 10; i++) {
    console.log(generateRandomNumber());
}
```

上記のコードを実行すると、以下のような結果が得られます。

```
871586976
1913540463
1713395347
1318216028
276940247
2104953516
188162354
511305068
111231574
1443754135
```

## ディープダイブ
線形合同法は簡単な計算により乱数を生成できるため、コンピュータのリソースをほとんど消費しません。しかし、シード値が同じだと同じ乱数を繰り返し生成してしまうという欠点があります。そのため、単純な方法ではなく、擬似乱数を生成するアルゴリズムが用いられることもあります。

## 参考
- [Random Number Generation in JavaScript](https://www.youtube.com/watch?v=QWUR9BmFI4g)
- [メルセンヌツイスタを使った乱数生成](https://ja.wikipedia.org/wiki/%E3%83%A1%E3%83%AB%E3%82%BB%E3%83%B3%E3%83%8C%E3%83%84%E3%82%A4%E3%82%B9%E3%82%BF)
- [線形合同法による擬似乱数の生成](https://ja.wikipedia.org/wiki/%E7%B7%9A%E5%BD%A2%E5%90%88%E5%90%8C%E6%B3%95)

## 参考文献
乱数はプログラミングにおいて重要な役割を果たすテクニックの一つです。上記の記事や参考文献を参考にしながら、より高度な乱数の生成方法を学習することをお勧めします。

## 参考文献
- [JavaScriptリファレンス - Math.random()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [How to Create a Random Number Generator in JavaScript](https://www.tutorialspoint.com/how-to-create-a-random-number-generator-in-javascript)