---
title:                "「HTMLの解析」"
html_title:           "C: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTML ページからデータを取得するためには、HTML をパースすることが必要になります。C 言語のパーサーを使用することで、高速かつ堅牢な方法で HTML をパースすることができます。

## 方法

まず、C 言語のパーサーをインポートします。

```C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
```

次に HTML ファイルを開き、その内容を文字列として読み込みます。

```C
FILE *html_file = fopen("test.html", "r");
char *html_string = NULL;
long html_size;

if (html_file)
{
  // ファイルサイズを取得する
  fseek(html_file, 0, SEEK_END);
  html_size = ftell(html_file);
  fseek(html_file, 0, SEEK_SET);

  // メモリを確保し、ファイル内容をコピーする
  html_string = malloc(html_size + 1);
  if (html_string)
    fread(html_string, 1, html_size, html_file);
  fclose(html_file);
}

html_string[html_size] = '\0';
```

これで、HTML 文字列を取得することができました。次に、この文字列をパースして必要な情報を取得します。例えば、"title" タグ内のテキストを取得する場合は、以下のようにします。

```C
// "title" タグを検索する
char *title_start = strstr(html_string, "<title>");
char *title_end = strstr(html_string, "</title>");

if (title_start && title_end)
{
  // タイトルの文字列を取得する
  // "<title>" の次の位置から "</title>" の前までをコピーする
  size_t title_length = title_end - title_start - 7;
  char *title = strndup(title_start + 7, title_length);
  printf("Title: %s\n", title);
}
```

このように、HTML 文字列をパースすることで、必要な情報を取得することができます。

## ディープダイブ

C 言語のパーサーは非常に高速かつメモリ効率が良く、大きな HTML ファイルでも問題なく処理することができます。しかし、パーサーを実装する際には、HTML の構造について詳しく理解する必要があります。HTML のタグや要素の意味を把握し、正確な位置を探すための処理を書く必要があります。

## 関連リンク

- [C言語のパーサーの作り方](https://levelup.gitconnected.com/creating-a-simple-parser-in-c-d7a9e74c4a0c)
- [C言語の文字列操作（strndup関数）](https://atmarkit.itmedia.co.jp/ait/articles/1708/17/news019.html)
- [よく使われるC言語のファイル操作関数](https://www.atmarkit.co.jp/ait/articles/1604/15/news016.html)