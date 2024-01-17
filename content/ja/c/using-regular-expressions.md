---
title:                "正規表現を使う"
html_title:           "C: 正規表現を使う"
simple_title:         "正規表現を使う"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

いつもコードを書いているときに、特定のパターンや文字列を検索する必要があることがあります。それを手作業で行うのは面倒だし、間違いが発生する可能性があります。そこで登場するのが「正規表現」です。これはパターンや文字列を検索し、変更するための強力なツールです。

## 何となぜ？

正規表現を使うと、プログラマーは特定のパターンや文字列を簡単に検索し、変更できます。これにより、時間と労力を節約できるだけでなく、間違いを少なくすることもできます。また、パターンや文字列を自動的に処理することで、より効率的なコードを作成できます。

## 方法：

正規表現を使用するには、<code>C ...</code>コードブロック内で次のように入力します。まず、「#include <regex.h>」を追加し、正規表現ライブラリをインポートします。次に、パターンを定義し、<code>regex_t</code>構造体に格納します。そして、検索する文字列を定義し、<code>regmatch_t</code>構造体に格納します。最後に、<code>regcomp()</code>関数と<code>regexec()</code>関数を使用して、パターンと文字列を比較して一致するかどうかを確認します。

```
#include <stdio.h>
#include <regex.h>

int main() {
    // パターンを定義
    char pattern[] = "abc";
    // 検索する文字列を定義
    char string[] = "defabcghi";
    
    int status;
    // regex_t構造体にパターンを格納
    regex_t regex;
    // regmatch_t構造体に一致した箇所を格納
    regmatch_t match;
    
    // パターンをコンパイル
    status = regcomp(&regex, pattern, 0);
    if (status != 0) {
        printf("パターンのコンパイルに失敗しました。\n");
        return 1;
    }
    
    // 文字列とパターンを比較
    status = regexec(&regex, string, 1, &match, 0);
    if (status == 0) {
        printf("文字列がパターンと一致しました。\n");
    } else {
        printf("文字列がパターンと一致しませんでした。\n");
    }
    
    // regex_t構造体を解放
    regfree(&regex);
    
    return 0;
}
```

上記の例では、パターン「abc」を検索すると、文字列「defabcghi」に一致することが分かります。

## 詳しく探る：

### 歴史的背景

正規表現は1960年代にアメリカの計算機科学者、ケン・トンプソンによって開発されました。彼は検索や置換のためのパターンマッチングを容易にするために、正規表現をUNIXの開発に取り入れました。その後、多くのプログラミング言語にも導入され、様々なアプリケーションで利用されています。

### 代替手段

正規表現には代替手段もあります。例えば、文字列操作関数やループ文を使用して特定のパターンを検索することもできます。しかし、これらの方法では複雑なパターンを扱うことができず、かなり手間がかかる場合があります。

### 実装の詳細

正規表現は、文字列とパターンを比較することで動作します。文字列はシーケンス(データの並び)であり、パターンはマッチングルールです。そして、比較結果に基づいて一致する部分を抽出します。

## 関連情報：

- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [C言語の最新バージョン](https://www.iso.org/standard/29237.html)