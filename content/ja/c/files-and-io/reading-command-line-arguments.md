---
title:                "コマンドライン引数の読み取り"
aliases: - /ja/c/reading-command-line-arguments.md
date:                  2024-02-03T18:06:32.478846-07:00
model:                 gpt-4-0125-preview
simple_title:         "コマンドライン引数の読み取り"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Cプログラミングにおいて、コマンドライン引数を読み取ることは、プログラムが端末から直接入力を受け付けることを可能にし、柔軟性と使いやすさを向上させます。プログラマーはこれを利用して、コードを修正することなくスクリプトの振る舞いを設定し、アプリケーションを適応性があり効率的なものにします。

## 方法：

Cでは、`main`関数を`int argc`と`char *argv[]`のパラメータを使ってコマンドライン引数を受け付けるように設計することができます。ここで、`argc`は渡された引数の数を表し、`argv`はすべての引数をリストする文字ポインタの配列です。以下に簡単な例を示します：

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Program Name: %s\n", argv[0]);
    printf("Number of Arguments: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

上記のコードを使用して、プログラムが`./programName -a example`として実行された場合、出力は以下のようになります：

```
Program Name: ./programName
Number of Arguments: 2
Argument 1: -a
Argument 2: example
```

これは、コマンドライン引数がCプログラム内でどのようにパースそして利用されるかを示しています。

## 深掘り

プログラムへの引数の渡し方はUnixの初期の時代までさかのぼります。この伝統的なアプローチでは、`argc`と`argv`はコマンドラインとのやり取りのためのシンプルでありながら強力なインターフェースを提供し、小さくモジュラーなユーティリティが協力して動作するUnixの哲学を体現しています。現代の言語ではより洗練されたライブラリやフレームワークがコマンドライン引数の解析のために導入されることが多いですが、Cの方法の直接性は透明性と制御の面で比類のないものを提供します。

最近の進展では、POSIXシステムの`getopt`のようなライブラリが、長いオプション名を扱うや引数が欠けている場合のデフォルト値をサポートするなど、より複雑な解析ニーズに対応するように進化しました。それでも、`argc`と`argv`の基本的な仕組みは、Cでのプログラムがランタイム環境とどのように相互作用するかを理解する上で不可欠です。

批評家は、`argc`と`argv`を直接扱うことはエラーを起こしやすいと主張し、より高レベルの抽象化の使用を推奨するかもしれません。それでも、Cの詳細とその低レベル操作の微妙さをマスターしようとする人々にとって、コマンドライン引数の解析をマスターすることは通過儀礼です。この歴史的な方法論と実用的な有用性の混合は、システムプログラミングとソフトウェア開発におけるCの持続的な魅力の多くを要約しています。
