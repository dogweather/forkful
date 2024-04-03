---
date: 2024-01-26 04:27:47.868785-07:00
description: "\uBC29\uBC95: XML\uC744 \uC0DD\uC131\uD558\uAE30 \uC704\uD574 `XMLWriter`\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uACE0, \uC774\uB97C \uD30C\
  \uC2F1\uD558\uAE30 \uC704\uD574 `tinyxml2` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD560 \uAC83\uC785\uB2C8\uB2E4. \uBA3C\uC800 \uC544\uB450\uC774\uB178\
  \ IDE\uC758 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uAD00\uB9AC\uC790\uB97C \uD1B5\uD574\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC124\uCE58\uD558\uC2ED\uC2DC\uC624. XML\
  \ \uBB38\uC11C \uC0DD\uC131."
lastmod: '2024-03-13T22:44:55.642707-06:00'
model: gpt-4-0125-preview
summary: "XML\uC744 \uC0DD\uC131\uD558\uAE30 \uC704\uD574 `XMLWriter` \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uACE0, \uC774\uB97C \uD30C\uC2F1\uD558\
  \uAE30 \uC704\uD574 `tinyxml2` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\
  \uD560 \uAC83\uC785\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 방법:
XML을 생성하기 위해 `XMLWriter` 라이브러리를 사용하고, 이를 파싱하기 위해 `tinyxml2` 라이브러리를 사용할 것입니다. 먼저 아두이노 IDE의 라이브러리 관리자를 통해 라이브러리를 설치하십시오.

XML 문서 생성:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Serial을 사용하여 출력
  
  xml.header();
  xml.tag("greeting").tag("text").text("안녕하세요, 세계!").close().close();
  xml.flush();
}

void loop() {
}
```

XML 문자열 디코딩:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>안녕하세요, 세계!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

샘플 출력:

```
<greeting>
  <text>안녕하세요, 세계!</text></greeting>
```

## 심층 탐구
XML, 또는 확장 가능 마크업 언어는 문서를 인간과 기계 모두가 읽을 수 있는 형식으로 인코딩하기 위한 일련의 규칙을 정의하는 마크업 언어입니다. 이는 90년대 후반부터 있어왔으며 플랫폼 독립적인 데이터 교환을 필요로 하는 다양한 분야에서 광범위하게 사용되고 있습니다. 아두이노의 제한된 메모리 자원으로 인해 PC에서보다 XML을 다루기가 더 도전적입니다. 따라서 경량 라이브러리가 중요합니다. JSON이 그보다 간단한 구문과 더 작은 크기 때문에 데이터 교환을 위해 인기를 얻고 있지만, XML은 특히 레거시 시스템을 다루거나 스키마를 통한 문서 유효성 검증이 필요한 애플리케이션에서 여전히 널리 사용됩니다. 아두이노 XML 구현의 핵심은 문서를 세그먼트로 읽어 메모리 사용을 낮게 유지하는 스트림 파싱입니다.

## 참고 자료
- [TinyXML-2 라이브러리 문서](https://leethomason.github.io/tinyxml2/)
- JSON 데이터를 다룰 때의 대안으로 [아두이노 JSON 라이브러리](https://arduinojson.org/)
- 일반적인 XML 학습을 위한 [W3Schools XML 튜토리얼](https://www.w3schools.com/xml/)
- 공식 XML 표준과 권장사항을 위한 [W3C XML 명세](https://www.w3.org/XML/)
