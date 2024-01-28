---
title:                "XML 다루기"
date:                  2024-01-26T04:35:55.387282-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML은 eXtensible Markup Language의 약어로, JSON의 장황한 사촌과 같습니다. 여러분은 레거시 시스템, 엔터프라이즈 소프트웨어 또는 JSON 대열에 오르지 않은 API를 다룰 때 XML과 씨름하게 될 것입니다. XML은 데이터 교환에서 여전히 그 가치를 지니고 있습니다.

## 어떻게:
Rust에서는 `xml-rs`와 같은 크레이트로 XML을 다룰 수 있습니다. `Cargo.toml`에 `xml-rs = "0.8"`을 추가하여 설치하세요. 다음은 간단한 XML을 파싱하는 방법입니다:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("시작: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("텍스트: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("종료: {}", name);
            }
            Err(e) => {
                println!("오류: {}", e);
            }
            _ => {}
        }
    }
}
```

출력:
```
시작: book
시작: title
텍스트: Rust in Action
종료: title
시작: author
텍스트: Tim McNamara
종료: author
시작: year
텍스트: 2021
종료: year
종료: book
```
이 코드는 XML을 스트림 읽기로 처리하며, 시작 및 종료 요소와 텍스트 데이터를 처리하고 각 단계를 기록합니다.

## 심층 탐구:
XML은 기술 분야에서 선배로, 90년대 후반 웹을 위해 만들어졌습니다. 그 설계는 가독성(기계와 인간 모두를 위해)과 방대한 자기 설명 데이터를 촉진합니다.

대안이 있나요? 물론입니다, JSON은 현대의 웹 API를 위한 가벼우면서도 소음이 적은 선택입니다. 한편 YAML은 깔끔한 레이아웃으로 설정을 위한 팬층을 확보했습니다. 하지만 XML은 곧 사라지지 않을 것입니다—그것의 기반 위에 건설된 거대한 인프라가 있으니까요.

내부적으로, Rust의 XML 파싱은 이터레이터 패턴에 의존하여 메모리 사용량을 낮추고 성능을 높입니다. JSON 처리에 익숙한 이들에게는 `serde-xml-rs`와 같은 크레이트가 serde 같은 경험을 제공하는 특전일 것입니다.

## 참고 자료:
Rust와 XML에 대한 자세한 정보는: 
- Rust의 serde 호환성을 위한 `serde-xml-rs`: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- 공식 Rust 문서(다시 읽는 것이 도움이 됩니다): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
