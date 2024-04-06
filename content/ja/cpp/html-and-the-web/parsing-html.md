---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:42.505608-07:00
description: "\u65B9\u6CD5\uFF1A C++\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306EHTML\u30D1\
  \u30FC\u30B9\u6A5F\u80FD\u304C\u3042\u308A\u307E\u305B\u3093\u3002Google\u306EGumbo-parser\u3084\
  \u540C\u69D8\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3053\
  \u3068\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u306FGumbo-parser\u3092\
  \u4F7F\u7528\u3057\u305F\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.360703-06:00'
model: gpt-4-0125-preview
summary: ''
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
