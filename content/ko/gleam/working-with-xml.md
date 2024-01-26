---
title:                "XML 다루기"
date:                  2024-01-26T04:31:19.871682-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML을 다루는 것은 구조화되어 널리 사용되는 형식 때문에 데이터 교환용으로 사용되는 XML 문서를 파싱, 조작, 생성하는 것을 포함합니다. 프로그래머들은 데이터의 범용 언어인 XML로 수많은 시스템과 인터페이스합니다.

## 방법:
Gleam은 기본적으로 XML을 지원하지 않으므로 `gleam_xml`과 같은 외부 라이브러리를 사용하게 됩니다. 먼저, `gleam.toml`에 추가하세요:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

이제, XML을 파싱하고 생성하세요:

```rust
import gleam/xml

// XML 파싱
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// XML 생성
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

`xml.render(node)`의 샘플 출력은 다음과 같습니다:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## 심층 탐구
XML은 W3C의 사양으로, HTML의 자매로 eXtensible Markup Language를 의미합니다. 1990년대 후반부터 존재해왔습니다. Gleam에서 XML을 다루는 것은 다소 시간을 거슬러 올라가는 느낌입니다. JSON과 Protocol Buffers가 더 유행하지만, XML은 여전히 업계의 레거시 시스템과 특정 산업에서 광범위하게 사용되므로 여전히 관련성이 있습니다.

Erlang 생태계에서는 `xmerl`과 같은 대안이 존재하지만, `gleam_xml` 라이브러리는 Gleam 사용자에게 더 관용적인 접근 방식을 제공합니다. 이것은 기존의 Erlang 라이브러리 위에 구축되었지만, Gleam 친화적인 API를 노출합니다. Gleam에서의 XML 접근 방식은 간소함과 안전성을 추구하며, 보일러플레이트를 줄이고 타입 안전성을 강조합니다.

구현 측면에서, `gleam_xml`을 포함한 XML 라이브러리는 일반적으로 DOM과 유사한 구조를 제공합니다. 이것은 노드, 속성, 중첩 요소를 포함하며, 잠재적으로 크고 복잡한 문서를 다루기 위해 Erlang의 패턴 매칭과 동시성 모델을 활용합니다.

## 참고자료
- Hex에서 `gleam_xml` 라이브러리: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- W3C의 공식 XML 표준: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- 종합적인 XML 튜토리얼: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- XML 처리를 위한 Erlang의 `xmerl` 문서: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)