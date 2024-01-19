---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱이란 HTML 문서를 분석하고 해당 정보를 추출하는 과정입니다. 프로그래머들은 이를 통해 웹페이지에서 필요한 데이터를 얻거나, 웹사이트의 구조를 이해하는 데 사용합니다.

## 어떻게:

```C
#include <stdio.h>
#include <tagc.h>

int main() {
    TAG *root, *tmp;

    /* HTML load */
    root = html_load("sample.html");
    if(!root) {
        printf("Cannot open file\n");
        return -1;
    }

    /* Search tag from HTML */
    tmp = html_search(root, "div");
    if(!tmp)
        printf("Tag not found\n");
    else
        printf("Tag %s found\n", tmp->name);

    /* Free Memory */
    html_free(root);

    return 0;
}
```

이 예제 코드는 `tagc`라이브러리를 사용하여 "div" 태그를 검색합니다. 성공하면 결과 출력하고, 실패하면 오류 메시지를 출력합니다.

## 깊이 들어가보기:

HTML 파싱은 웹의 초기 개발 초기부터 시작되었습니다. 크롤러, 스크래퍼 등 다양한 웹 프로그래밍 어플리케이션에 사용되어 왔습니다. C언어 외에도 파이썬, 자바 등 많은 언어가 HTML 파싱을 지원합니다.

HTML 파싱의 대체 방법으로는 CSS 선택자 사용, XPath 사용 등이 있지만 각가의 장단점이 있으며, 요구 사항에 따라 적합한 방법을 선택해야 합니다.

C언어에서 HTML 파싱의 구현에 관해서는, 효율적인 메모리 관리와 에러 처리가 중요합니다. 슬랙오버플로우나 메모리누수는 프로그램 전체에 치명적인 영향을 끼치기 때문입니다.

## 참고 자료:

1. 간편한 HTML 파서 소개: [Gumbo][1]
2. HTML 파싱에 대한 자세한 문서 [Mozilla MDN][2]

[1]: https://github.com/google/gumbo-parser
[2]: https://developer.mozilla.org/ko/docs/Web/HTML/Parser