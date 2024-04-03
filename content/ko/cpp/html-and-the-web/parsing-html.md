---
date: 2024-01-20 15:30:38.324382-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) C++\uC5D0\uC11C\uB294\
  \ \uC5EC\uB7EC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574 HTML\uC744\
  \ \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. `Gumbo-parser`\uB294 \uADF8\
  \uC911 \uD558\uB098\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.665078-06:00'
model: unknown
summary: "C++\uC5D0\uC11C\uB294 \uC5EC\uB7EC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD574 HTML\uC744 \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## How to: (어떻게 하나요?)
C++에서는 여러 라이브러리를 사용해 HTML을 파싱할 수 있습니다. `Gumbo-parser`는 그중 하나입니다.

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
       (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        std::cout << href->value << std::endl;
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href=\"https://example.com\">Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

샘플 출력:

```
https://example.com
```

## Deep Dive (깊이 들여다보기)
HTML 파싱은 웹의 초창기부터 필요했습니다. 초기에는 정규 표현식과 같은 단순한 도구를 사용했지만, HTML의 복잡성 때문에 전문적인 파서가 개발되었습니다. `Gumbo-parser`는 구글이 개발한 오픈소스 HTML5 파서로, C 언어로 작성되었습니다. 대안으로 `htmlcxx`, `MyHTML`, `Gumbo++` 등이 있습니다. 이 파서들은 각각 다르게 HTML을 DOM(Document Object Model) 트리로 변환하여 프로그램에서 사용할 수 있게 해 줍니다.

## See Also (더 보기)
- Gumbo-parser GitHub: https://github.com/google/gumbo-parser
- htmlcxx: http://htmlcxx.sourceforge.net/
- MyHTML GitHub: https://github.com/lexborisov/myhtml
- W3C HTML5 Parser Specifications: https://www.w3.org/TR/html5/syntax.html
