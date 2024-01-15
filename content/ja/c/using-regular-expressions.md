---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は、文字列の検索や置換など、テキスト操作をより簡単かつ効率的に行うためです。

## 使い方

正規表現はC言語で使用することができ、非常に便利です。以下に、正規表現を使用したコーディングの例を示します。

```C
#include <stdio.h>
#include <regex.h>
 
int main()
{
    // 正規表現パターンを定義
    char *pattern = "c.*g";
    // 検索する文字列を定義
    char* str = "coding";
    // 正規表現オブジェクトを作成
    regex_t regex;
    // 正規表現をコンパイル
    int compile_err = regcomp(&regex, pattern, 0);
    // エラー処理
    if (compile_err) {
        printf("正規表現が無効です\n");
        return 1;
    }
    // コンパイルした正規表現を使用して文字列を検索
    int match = regexec(&regex, str, 0, NULL, 0);
    // マッチした場合は0を返す
    if (!match)
        printf("%sは正規表現にマッチしました\n", str);
    else
        printf("%sは正規表現にマッチしませんでした\n", str);
    // 正規表現オブジェクトを解放
    regfree(&regex);
    return 0;
}
```

以下は上記コードの実行結果です。

```
codingは正規表現にマッチしました
```

## 厳密なマッチング

正規表現では、特定の文字やパターンをより厳密に検索することもできます。例えば、大文字小文字を区別したり、特定の文字が含まれているかどうかをチェックしたりすることができます。さらに詳細な情報は正規表現のドキュメントを参照してください。

## 詳細情報

正規表現は非常に強力なテキスト処理ツールですが、使い方を間違えると予期せぬ結果を生む場合があります。必ずドキュメンテーションを参照し、正しいパターンを使用するようにしましょう。

## 参考リンク

- [C言語で正規表現を利用する方法](https://qiita.com/moriyaman/items/0a76a0d12ef253dd7b35)
- [正規表現の基礎](https://ja.wikipedia.org/wiki/%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE)
- [正規表現のチュートリアル](https://www.w3schools.com/cpp/cpp_regex.asp)

## 関連リンク

- [C言語の基礎：文字列の操作](https://qiita.com/ban/\_awake/items/c1f1228943654c2f2583)
- [C言語の基礎：配列とポインタ](https://qiita.com/ban/\_awake/items/4935a5f00400158552d5)