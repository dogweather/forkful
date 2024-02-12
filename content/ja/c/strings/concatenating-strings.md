---
title:                "文字列の連結"
aliases:
- /ja/c/concatenating-strings/
date:                  2024-02-03T17:54:06.355927-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列の連結"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?

Cでの文字列の連結は、2つ以上の文字列を端から端まで結合して新しい文字列を形成する操作を指します。プログラマーは、実行時に動的に文字列を構築するためにこの操作を行います。これは、意味のあるメッセージ、ファイルパス、または様々な文字列ソースから組み立てられた任意のデータを作成するのに不可欠です。

## 方法:

Cでは、文字列はヌル文字（`\0`）で終わる文字の配列です。高水準言語とは異なり、Cには組み込みの文字列連結関数が提供されていません。代わりに、`<string.h>`ライブラリの`strcat()`関数または`strncat()`関数を使用します。

`strcat()`を使用した簡単な例をこちらです:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";

    strcat(destination, source);

    printf("%s\n", destination);  // 出力: Hello, World!
    return 0;
}
```

`strcat()`関数は2つの引数を取ります: 連結結果を保持するのに十分な空間を持つ宛先文字列と、ソース文字列です。それから、ソース文字列を宛先に追加します。

文字の数をより詳細に制御するには、`strncat()`の使用が安全です:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";
    int num = 3; // 追加する文字の数

    strncat(destination, source, num);

    printf("%s\n", destination);  // 出力: Hello, Wor
    return 0;
}
```

これは、ソース文字列の最初の`num`文字に連結を制限し、バッファオーバーフローを防ぐのに役立ちます。

## 詳細分析

`strcat()`関数と`strncat()`関数は、C言語の標準ライブラリの一部としてその発足以来存在しており、文字列やメモリの手動管理を必要とする言語の低レベル性質を反映しています。多くの現代のプログラミング言語が文字列をビルトインの連結演算子（`+`や`.concat()`など）を備えたファーストクラスオブジェクトとして扱うのとは対照的に、Cのアプローチはポインター、メモリ割り当て、バッファオーバーフローなどの潜在的な落とし穴へのより深い理解を必要とします。

`strcat()`と`strncat()`は広く使用されていますが、慎重に使用されない場合にセキュリティの脆弱性を生み出す可能性があるとしてしばしば批判されています。データが割り当てられたメモリを超えるバッファオーバーフローは、クラッシュを引き起こすか、任意のコード実行に悪用される可能性があります。その結果、プログラマーは、宛先文字列のサイズに基づいて書き込まれる文字数を制限し、より予測可能な振る舞いを提供する`snprintf()`のような安全な代替手段に increasingly more 傾いています:

```c
char destination[50] = "Hello, ";
char source[] = "World!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

この方法はより冗長ですが、簡潔さよりもセキュリティと堅牢性を優先するCプログラミング慣行へのシフトを示しています。

これらの課題にもかかわらず、Cでの文字列連結はC言語での効果的なプログラミングに不可欠な基本技能であり、そのニュアンスと関連するリスクを理解することはCプログラミングを習得する鍵です。
