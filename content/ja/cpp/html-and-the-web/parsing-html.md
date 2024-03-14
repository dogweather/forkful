---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:42.505608-07:00
description: "HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u3068\u306F\u3001HTML\u30B3\
  \u30F3\u30C6\u30F3\u30C4\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u7406\u89E3\u3057\
  \u3001\u64CD\u4F5C\u3067\u304D\u308B\u4F55\u304B\u306B\u5206\u89E3\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u305F\u3081\u3001\u30B3\
  \u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u3001\u307E\u305F\
  \u306F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u30A6\u30A7\u30D6\u30B9\
  \u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3092\u7D71\u5408\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.548906-06:00'
model: gpt-4-0125-preview
summary: "HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u3068\u306F\u3001HTML\u30B3\u30F3\
  \u30C6\u30F3\u30C4\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u7406\u89E3\u3057\u3001\
  \u64CD\u4F5C\u3067\u304D\u308B\u4F55\u304B\u306B\u5206\u89E3\u3059\u308B\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u305F\u3081\u3001\u30B3\u30F3\
  \u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u30A6\u30A7\u30D6\u30B9\u30AF\
  \u30EC\u30A4\u30D4\u30F3\u30B0\u3092\u7D71\u5408\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
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
