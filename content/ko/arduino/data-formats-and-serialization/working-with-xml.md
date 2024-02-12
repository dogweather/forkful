---
title:                "XML 다루기"
aliases:
- /ko/arduino/working-with-xml.md
date:                  2024-01-26T04:27:47.868785-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
아두이노에서 XML을 다루는 것은 웹 API나 구성 파일에서 오는 XML 데이터를 파싱하고 조작하는 것을 포함합니다. 프로그래머들은 데이터 교환을 위해 XML을 사용하는 서비스와 통합하거나 데이터를 구조화되고 사람이 읽을 수 있는 형식으로 저장하기 위해 이 작업을 합니다.

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
