---
date: 2024-01-26 04:30:36.879567-07:00
description: "\uBC29\uBC95: Fish\uB294 \uB0B4\uC7A5 XML \uD30C\uC2F1 \uAE30\uB2A5\uC774\
  \ \uC5C6\uC73C\uBBC0\uB85C `xmllint` \uB610\uB294 `xmlstarlet`\uACFC \uAC19\uC740\
  \ \uC678\uBD80 \uB3C4\uAD6C\uC5D0 \uC758\uC874\uD574\uC57C \uD569\uB2C8\uB2E4. \uAC12\
  \uC744 \uC77D\uAE30 \uC704\uD55C \uCF54\uB4DC \uC870\uAC01\uC740 \uB2E4\uC74C\uACFC\
  \ \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.891070-06:00'
model: gpt-4-0125-preview
summary: "Fish\uB294 \uB0B4\uC7A5 XML \uD30C\uC2F1 \uAE30\uB2A5\uC774 \uC5C6\uC73C\
  \uBBC0\uB85C `xmllint` \uB610\uB294 `xmlstarlet`\uACFC \uAC19\uC740 \uC678\uBD80\
  \ \uB3C4\uAD6C\uC5D0 \uC758\uC874\uD574\uC57C \uD569\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 방법:
Fish는 내장 XML 파싱 기능이 없으므로 `xmllint` 또는 `xmlstarlet`과 같은 외부 도구에 의존해야 합니다. 값을 읽기 위한 코드 조각은 다음과 같습니다:

```fish
# xmlstarlet을 사용하여 XML 파싱
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

출력:
```
Hello World
```

XML을 편집하려면 다음을 사용하십시오:

```fish
# xmlstarlet을 사용하여 XML 요소 편집
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

출력:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## 심층 분석:
XML은 90년대 말부터 사용되어 왔으며, 가독성 및 기계 친화성을 위해 만들어졌습니다. 간단함 때문에 JSON이 XML의 인기를 어느 정도 대체했지만, 문서 유효성 검사와 네임스페이스가 중요한 곳에서는 XML이 깊이 자리 잡고 있습니다.

대안이 있나요? 물론입니다—성능 집약적인 앱을 위한 JSON, YAML 또는 Protocol Buffers 같은 이진 형식 등이 있습니다. 하지만 XML의 스키마와 XML 변환을 위한 XSLT는 견고함이 중요한 복잡한 시나리오에서 결정적인 요소가 될 수 있습니다.

내부적으로, `xmlstarlet`과 같은 도구는 libxml2와 같은 강력한 라이브러리를 감싸고 있으며, XPath와 XQuery를 통해 세밀한 XML 조작을 할 수 있게 해줍니다. 이들은 단순한 XML 도구가 아니라 XML에 접근하는 어떤 언어에서도 비슷한 개념을 적용할 때 DOM 조작으로의 게이트웨이입니다.

## 참조:
- [xmlstarlet 문서](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish 문서](https://fishshell.com/docs/current/index.html)
- [XPath 및 XQuery 함수 및 연산자](https://www.w3.org/TR/xpath-functions/)
