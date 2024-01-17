---
title:                "ランダムな数字を生成する"
html_title:           "C++: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

＃＃ 何か＆なぜ？
ランダムな数字を生成することはプログラマーによって行われることであり、その中には特定の範囲内のランダムな整数または少数を生成することも含まれます。これは、乱数を使用してゲームやシミュレーション、または暗号アルゴリズムなどさまざまなアプリケーションで使用されることができます。

＃＃ ハウツー：
＃ C++コードを使用した例：
```
using namespace std;
#include <iostream>
#include <cstdlib>

int main() {
    // 1から10までのランダムな整数を生成する
    int random_int = rand() % 10 + 1;
    cout << "Random integer between 1 and 10: " << random_int << endl;

    // 0から1までのランダムな少数を生成する
    float random_float = rand() / (float)RAND_MAX;
    cout << "Random float between 0 and 1: " << random_float << endl;
}
```
＃ 出力：
```
Random integer between 1 and 10: 7
Random float between 0 and 1: 0.32564
```

＃＃ 深堀り：
乱数生成の歴史は広く、遠くまで遡ることができます。古代ギリシャや中国など、さまざまな文明においてすでに乱数生成は用いられていました。今日では、擬似乱数生成アルゴリズムが一般的に使用されています。これは、ある種の数学的計算に基づいてランダムな数列を生成するものです。

代替手段としては、ハードウェア乱数生成器があります。これは、物理的なプロセスに基づいて真の乱数を生成することができますが、ソフトウェア乱数生成器ほど高速ではありません。

C++の標準ライブラリには、擬似乱数生成に使用するための様々な関数と生成器が用意されており、一般的に使用されています。

＃＃ 関連情報：
 [C++ Reference: rand()](https://www.cplusplus.com/reference/cstdlib/rand/)