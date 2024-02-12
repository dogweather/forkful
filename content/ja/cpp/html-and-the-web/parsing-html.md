---
title:                "HTMLの解析"
aliases:
- /ja/cpp/parsing-html.md
date:                  2024-02-03T19:11:42.505608-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
HTMLをパースするとは、HTMLコンテンツをプログラムが理解し、操作できる何かに分解することを意味します。プログラマーは、データを抽出するため、コンテンツを操作するため、またはアプリケーションにウェブスクレイピングを統合するためにこれを行います。

## 方法：
C++には組み込みのHTMLパース機能がありません。GoogleのGumbo-parserや同様のライブラリを使用することがよくあります。以下はGumbo-parserを使用した簡単な例です：

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

サンプル出力：
```
https://example.com
```

## 掘り下げ
C++でのHTMLパースは常に簡単でしたというわけではありません。歴史的に、プログラマは正規表現や手書きのパーサーを使用していましたが、どちらもエラーが発生しやすく、面倒です。現在では、Gumbo-parserのような堅牢なライブラリがパースの微妙なところを処理してくれるため、より簡単で信頼性が高くなりました。

代替手段には、Tidy、MyHTML、あるいはC++の`system`関数や組み込みインタプリタを介してPythonのBeautifulSoupとの統合も含まれます。

実装において、これらのライブラリはHTMLをドキュメントオブジェクトモデル（DOM）ツリーに変換します。「方法」セクションで示されたように、DOMを走査し操作することで、ユーザーはデータを抽出し作業することができます。

## 参照
- [Gumbo-parser GitHubリポジトリ](https://github.com/google/gumbo-parser)
- [HTML解析ライブラリのリスト](https://en.cppreference.com/w/c/experimental/dynamic)
- [C++とPythonの相互運用性](https://docs.python.org/3/extending/embedding.html)
