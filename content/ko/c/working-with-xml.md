---
title:                "XML 다루기"
date:                  2024-01-26T04:28:27.987552-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
C에서 XML을 다루는 것은 XML 파일을 파싱, 생성, 그리고 조작하는 것을 포함하며 - 기본적으로 구조화된 데이터 저장소입니다. 프로그래머들은 이러한 작업을 설정, 데이터 교환 등을 위해 이동 가능하고 인간이 읽을 수 있는 형식으로 데이터와 상호작용하기 위해 수행합니다.

## 방법:
다음은 `libxml2` 라이브러리를 사용하여 XML 파일을 파싱하고 루트 요소를 가져오는 코드 조각입니다.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // XML 파일 파싱
    doc = xmlReadFile("example.xml", NULL, 0);

    // 루트 요소 가져오기
    root_element = xmlDocGetRootElement(doc);

    printf("루트 요소: %s\n", root_element->name);

    // 문서 해제
    xmlFreeDoc(doc);

    // 파서 정리
    xmlCleanupParser();

    return 0;
}
```

루트가 `<data>`인 XML의 샘플 출력은 다음과 같습니다:
```
루트 요소: data
```

## 심층 탐구
XML, 또는 Extensible Markup Language는 90년대 후반으로 거슬러 올라가며 데이터를 기술하고 구조화하는 방법을 제공합니다. C에서는 `libxml2`가 주요 선택지입니다. 이는 견고하지만 XML 초보자에게는 가장 쉽지 않습니다. 대안으로는 `tinyxml2`가 있으며, 이는 더 가볍고 초보자에게 친숙합니다. 구현과 관련하여, C는 기본적인 XML 지원을 갖추고 있지 않으므로 라이브러리가 이 공백을 메웁니다. 이들은 크기, 속도, 복잡성 및 이식성에 있어 다양합니다. 대부분 DOM 및 SAX 파싱 방법을 제공합니다: DOM은 전체 문서를 메모리에 로드하는 것이 좋으며, 작은 문서에 유용; SAX는 이벤트 주도로, 요소를 실시간으로 처리하며, 큰 파일에 더 나은 선택입니다. 둘 다 사용 사례와 트레이드오프가 있습니다.

## 참고자료
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 on GitHub](https://github.com/leethomason/tinyxml2)
- [w3schools의 XML 튜토리얼](https://www.w3schools.com/xml/)
- [W3C의 XML 명세](https://www.w3.org/XML/)
