---
date: 2024-01-26 04:30:36.879567-07:00
description: "XML\uACFC \uC791\uC5C5\uD55C\uB2E4\uB294 \uAC83\uC740 \uAD6C\uC131,\
  \ \uBA54\uC2DC\uC9D5 \uB4F1\uC5D0\uC11C \uC0AC\uC6A9\uB418\uB294 \uBCF4\uD3B8\uC801\
  \uC774\uACE0 \uAD6C\uC870\uD654\uB41C \uD615\uC2DD\uC758 \uB370\uC774\uD130\uB97C\
  \ \uCC98\uB9AC\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uC77D\uACE0, \uC4F0\uACE0\
  , \uC5C5\uB370\uC774\uD2B8\uD558\uACE0, \uC870\uD68C\uD558\uB294 \uC791\uC5C5\uC744\
  \ \uD1B5\uD574 \uC218\uB9CE\uC740 \uC571\uACFC \uC11C\uBE44\uC2A4\uC5D0\uC11C \uC0C1\
  \uD638 \uC6B4\uC6A9\uC131\uC744 \uC704\uD574 XML\uC744 \uC870\uC791\uD569\uB2C8\uB2E4\
  ."
lastmod: 2024-02-19 22:05:14.806342
model: gpt-4-0125-preview
summary: "XML\uACFC \uC791\uC5C5\uD55C\uB2E4\uB294 \uAC83\uC740 \uAD6C\uC131, \uBA54\
  \uC2DC\uC9D5 \uB4F1\uC5D0\uC11C \uC0AC\uC6A9\uB418\uB294 \uBCF4\uD3B8\uC801\uC774\
  \uACE0 \uAD6C\uC870\uD654\uB41C \uD615\uC2DD\uC758 \uB370\uC774\uD130\uB97C \uCC98\
  \uB9AC\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uC77D\uACE0, \uC4F0\uACE0, \uC5C5\
  \uB370\uC774\uD2B8\uD558\uACE0, \uC870\uD68C\uD558\uB294 \uC791\uC5C5\uC744 \uD1B5\
  \uD574 \uC218\uB9CE\uC740 \uC571\uACFC \uC11C\uBE44\uC2A4\uC5D0\uC11C \uC0C1\uD638\
  \ \uC6B4\uC6A9\uC131\uC744 \uC704\uD574 XML\uC744 \uC870\uC791\uD569\uB2C8\uB2E4\
  ."
title: "XML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML과 작업한다는 것은 구성, 메시징 등에서 사용되는 보편적이고 구조화된 형식의 데이터를 처리하는 것을 의미합니다. 프로그래머들은 데이터를 읽고, 쓰고, 업데이트하고, 조회하는 작업을 통해 수많은 앱과 서비스에서 상호 운용성을 위해 XML을 조작합니다.

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
