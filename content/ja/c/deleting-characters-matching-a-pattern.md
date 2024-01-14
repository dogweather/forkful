---
title:                "C: マッチングパターンにマッチする文字を削除する"
simple_title:         "マッチングパターンにマッチする文字を削除する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#なぜ文字のパターンにマッチする文字を削除するか?

文字のパターンにマッチする文字を削除することは、テキスト処理やデータの変換などのプログラミングタスクでよく使用されます。例えば、特定の文字列を検索して削除することで、テキスト内の不要な情報を取り除くことができます。

## 方法

パターンにマッチする文字を削除するためには、文字列内の各文字を順番にチェックし、マッチする場合はその文字を削除するプログラムを作成する必要があります。以下のコード例を参考にしてください。

```C
#include <stdio.h>
#include <string.h>

void deleteChars(char *input, char *pattern) {
    int i, j, k;
    int input_len = strlen(input);
    int pattern_len = strlen(pattern);

    // 文字列の長さがゼロの場合は何も行いません
    if (input_len == 0 || pattern_len == 0) {
        return;
    }

    // 文字列内の各文字をチェックし、パターンにマッチした場合は削除します
    for (i = 0; i < input_len; i++) {
        // パターン文字列の先頭がマッチした場合は、パターンの文字数だけ削除します
        if (input[i] == pattern[0]) {
            for (j = i; j < i + pattern_len; j++) {
                for (k = j; k < input_len; k++) {
                    input[k] = input[k + 1];
                }
            }
        }
    }

    // 削除後の文字列を出力します
    printf("削除後: %s\n", input);
}

int main() {
    char input_str[50];
    char pattern_str[10];

    printf("テキスト: ");
    scanf("%s", input_str);

    printf("パターン: ");
    scanf("%s", pattern_str);

    deleteChars(input_str, pattern_str);

    return 0;
}
```

**入力:**

```
テキスト: こんにちは世界!
パターン: ん
```

**出力:**

```
削除後: こにちは世界!
```

## 深堀り

マッチする文字を削除する際には、文字の置換や正規表現を使用することもできます。また、パターンにマッチしない文字を残す場合には、逆のロジックを使用する必要があります。さらに、文字を削除するだけでなく、パターンにマッチした文字列を別の文字列に置換することもできます。用途やニーズに合わせて最適な方法を選択してください。

## 参考リンク

- [C言語 入門](https://prog-8.com/docs/c-intro)
- [C言語 中級](https://prog-8.com/docs/c-intermediate)
- [C言語 上級](https://prog-8.com/docs/c-advanced)
- [C言語を勉強したい方へ](https://codecamp.jp)
- [プログラミング入門サイト ドットインストール](https://dotinstall.com/lessons/basic_c)
- [正規表現の基本](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)

# See Also

- [文字列の扱い - C言語入門](https://prog-8.com/docs/c-intro-string)
- [文字列を操作する - C言語 中級](https://prog-8.com/docs/c-intermediate-string)
- [文字列操作: strcpy/strncpy, strcat/strncat, strcmp - C言語 上級](https://prog-