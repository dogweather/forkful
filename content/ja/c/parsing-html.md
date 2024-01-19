---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/parsing-html.md"
---

{{< edit_this_page >}}

# HTMLパージングの何となぜ？
HTMLパーシングとは、HTML文書を解析しデータを抽出するプロセスのことです。プログラマはHTMLパージングを使用してウェブページから情報を引き出し、データ分析やwebクローリングの作業を可能にします。

# 方法:
GitHubで公に利用できるGumboというライブラリを使ってHTMLのパーサを作りましょう。 Gumboは純粋なCライブラリで、Googleによってメンテナンスされています。以下にその一例を示します。

```C
#include <stdio.h>
#include <gumbo.h>
#include <assert.h>

static void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
        (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("%s\n", href->value);
    }

    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<a href='http://example.com'>Hello, world!</a>");
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```
これにより、「http://example.com」と出力されます。

# 深掘り:
HTMLパーシングは、ユーザーがウェブサイトから必要な情報を取得できるようにするために発明されました。初期のパーサは独自の実装をしていましたが、代わりに標準化された方法でHTMLを解析するための新しいライブラリが開発されるようになりました。

HTMLパーシングの代替手段としては、JavaScriptやPythonなどの他の言語を使用する方法があります。これらの言語はHTMLを解析するために多くのライブラリを持っています。

Gumboの実装では、トークンの抽出からDOMツリーの生成までが行われます。これにより、フロントエンドとバックエンド両方で利用できるデータ構造が生成されます。

# 参考資料:
- Gumbo GitHubリポジトリ: https://github.com/google/gumbo-parser
- PythonによるHTMLパーシング: https://docs.python.org/3/library/html.parser.html
- JavaScriptによるHTMLパーシング: https://developer.mozilla.org/ja/docs/Web/API/DOMParser