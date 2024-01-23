---
title:                "HTML 파싱"
date:                  2024-01-20T15:30:08.761951-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

HTML 파싱은 HTML 문서에서 데이터를 추출하고 그 구조를 이해하게요. 이걸 하는 이유는 웹 스크래핑, 웹 컨텐츠 분석, 자동화된 데이터 수집 등을 하기 위해서죠.

## How to: (어떻게 하나요?)

C에서 HTML 파싱을 할 때는 외부 라이브러리가 필요해요. 예를 들어 `libxml2`를 사용해봅시다.

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc;
    htmlNodePtr cur;

    // HTML 문자열
    char *html = "<html><body><p>Hello, World!</p></body></html>";

    // HTML 파싱
    doc = htmlReadMemory(html, strlen(html), NULL, NULL, 0);
    cur = xmlDocGetRootElement(doc);

    // 노드 순회하며 내용 출력
    for (cur = cur->children; cur; cur = cur->next) {
        if (cur->type == XML_ELEMENT_NODE) {
            printf("Element: %s\n", cur->name);
        }
    }

    // 메모리 해제
    xmlFreeDoc(doc);
    return 0;
}
```

출력 예제:
```
Element: body
Element: p
```

## Deep Dive (심층 분석)

파싱은 웹 초기부터 중요했어요. 웹이 커지면서 데이터 추출이 더 중요해졌죠. C에서 HTML을 파싱하는 데 다른 방법도 있어요. 예를 들면 `Gumbo` 파서나 `Tidy` 라이브러리 같은 거죠. `libxml2`는 XML과 HTML 모두를 위한 강력한 C 라이브러리에요. 보통 퍼포먼스가 중요하고 외부 의존성이 적은 환경에서 쓰이죠. 파싱 로직을 직접 구현하는 것보다 이 방법이 안전하고 빠릅니다.

## See Also (추가 정보)

- libxml2 공식 문서: http://xmlsoft.org/html/libxml-HTMLparser.html
- HTML5 파싱 라이브러리 Gumbo: https://github.com/google/gumbo-parser
- HTML Tidy 프로젝트: http://www.html-tidy.org/
