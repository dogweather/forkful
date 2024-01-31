---
title:                "リファクタリング"
date:                  2024-01-26T01:17:14.322758-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/refactoring.md"
---

{{< edit_this_page >}}

## 何を、なぜ？
リファクタリングは、既存のコンピュータコードの構造を変更するプロセスであり、その外部的な振る舞いを変えることなく行われます。プログラマーは、可読性を向上させたり、複雑さを減らしたり、コードをより保守しやすく、スケーラブルにするためにリファクタリングを行います。これは、将来的に多大な時間と頭痛の種を節約することができます。

## 方法：
いくつかのコードを整えましょう。配列内の整数の平均を計算する関数があると想像してください。一見すると、少し複雑なものに見えます。

**リファクタリング前：**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // forループの条件内で合計を計算する、いたずら！
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Average: %f\n", calculateStuff(array, length));

    return 0;
}
```

**リファクタリング後：**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Average: %f\n", calculateAverage(array, length));
    return 0;
}
```
この単純な例でさえ、関数を分割することでコードがクリーナーで、より保守しやすくなることが分かります。各関数には今、単一の責任があります - クリーンコーディングの重要な原則です。

## ディープダイブ
「リファクタリング」という用語は、特に90年代後半に、Martin Fowlerの著書「リファクタリング: 既存のコードのデザインを改善する」の出版とともに広まりました。リファクタリングはバグの修正や新機能の追加を意味するのではなく、コードの構造を改善することについてです。

リファクタリングを自動化するために役立つ洗練されたツールやIDE（統合開発環境）が多くあります。たとえば、CおよびC++のためのCLionなどがありますが、何が行われているかを理解することが不可欠です。

リファクタリングの代替手段には、スクラッチからコードを書き直す（リスキーで、しばしば必要ない）や、技術的負債をそのままにしておく（長い目で見ると、よりコストがかかる場合がある）などがあります。実装の詳細はプロジェクトによって異なりますが、一般的なリファクタリングには、可読性のための変数の名前変更、大きな関数の分割、マジックナンバーの名前付き定数への置き換えなどが含まれます。

また、DRY（Don't Repeat Yourself：同じことを繰り返さない）やSOLIDの原則などのパターンが、テスト、理解、共同作業がしやすいコードベースへ向けたリファクタリングの旅をガイドすることができます。

## 参照
リファクタリングの海をさらに深く探求するためには、以下をチェックしてください：

- Martin Fowlerのホームページ: https://martinfowler.com/ では、リファクタリングやソフトウェアデザインに関する数々の記事やリソースが宝の山のように提供されています。
- Refactoring.com: https://refactoring.com/ では、リファクタリング技法の例やカタログが提供されています。
- 「リファクタリング」の本: リファクタリングに関する聖書と考えられており、その方法論の完全なビューを提供します。
- Robert C. Martin著「クリーンコード: アジャイルソフトウェア職人技」では、理解しやすく保守しやすいコードの書き方について語られています。
