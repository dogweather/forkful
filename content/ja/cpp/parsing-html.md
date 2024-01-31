---
title:                "HTMLの解析"
date:                  2024-01-20T15:30:37.344539-07:00
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTML解析は、HTMLドキュメントの内容と構造を理解し使用するプロセスです。プログラマーは、ウェブページのデータを取得、加工、またはチェックするためにこれを行います。

## How to: (方法)
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
  const char* html = "<html><body><a href='https://example.com'>Visit Example.com!</a></body></html>";
  GumboOutput* output = gumbo_parse(html);

  search_for_links(output->root);

  gumbo_destroy_output(&kGumboDefaultOptions, output);
  return 0;
}
```
出力:
```
https://example.com
```

## Deep Dive (深掘り)
HTML解析は歴史的に難しい。HTMLは許容的な言語で、ブラウザは不完全や誤ったマークアップを修正します。そのため、パーサーも同様の修正力を持つ必要があります。解析方法にはDOMベース、SAXベース、レギュラーエクスプレッションベースなどがありますが、複雑なHTMLにはDOMやSAXを推奨します。GumboはGoogleのC言語ライブラリで、容易にDOMツリーを生成します。しかし、パフォーマンスや特定のニーズに応じた他のライブラリやツールも検討してください。

## See Also (さらに見る)
- Gumbo Parser: https://github.com/google/gumbo-parser
- HTML Standard: https://html.spec.whatwg.org/
- Comparison of HTML parsers: https://en.wikipedia.org/wiki/Comparison_of_HTML_parsers
