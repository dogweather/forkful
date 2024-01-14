---
title:                "C++: ランダム数字の生成"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜランダムな数字を生成するか

ランダムな数字を生成することは、コンピュータプログラムの多くで一般的に使用されます。例えば、ゲームやシミュレーション、データ暗号化などです。ランダムな数字を生成することで、プログラムがよりリアルな動作をすることができます。

## やり方

まずはランダムな数字を生成するために使用するヘッダーファイルをインクルードします。

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>
```

次に、乱数を生成するために `rand()`関数を使用します。この関数は、`<cstdlib>` ヘッダーに定義されています。また、プログラムが実行されるたびに異なる結果を得るために、 `srand()` 関数を使用して乱数のシード値を設定する必要があります。シード値には `time()` 関数を使用して現在の時刻を取得することができます。

```C++
int main(){
    // 乱数のシード値を設定
    srand(time(0));

    // 0から99までのランダムな数字を生成し出力
    int random = rand() % 100;
    std::cout << "ランダムな数字: " << random << std::endl;

    return 0;
}

```
サンプル出力: `ランダムな数字: 27`

## ランダムな数字を生成する際の深堀り

ランダムな数字を生成する際に、プログラマーが注意する必要がある点がいくつかあります。まず、乱数のシード値を設定する際には、 `time()` 関数のように、毎回異なる値を得られるようにすることが重要です。また、 `rand()` 関数は擬似乱数を生成するため、真のランダムとは異なることにも注意が必要です。

## 参考文献

- [C++でのランダム数字生成方法](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [rand()関数の詳細](https://www.cplusplus.com/reference/cstdlib/rand/)
- [srand()関数の詳細](https://www.cplusplus.com/reference/cstdlib/srand/)
- [time()関数の詳細](https://www.cplusplus.com/reference/ctime/time/)

## 関連記事

- [C++についてわかりやすく解説](https://www.sejiaeek.co.jp/blog/?p=875)
- [C++プログラミングの基礎](https://www.trans-it.net/technology/cpp.html)
- [C++のランダムな数字生成方法まとめ](https://programming.pc-note.net/cpp/rand.html)