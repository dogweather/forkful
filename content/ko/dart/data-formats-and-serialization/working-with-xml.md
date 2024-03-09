---
title:                "XML과 함께 작업하기"
date:                  2024-03-08T21:57:44.508845-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

Dart에서 XML과 작업하는 것은 XML 문서를 파싱하고, 조회하고, 수정하는 과정을 포함하며, 이는 웹 서비스, 설정 파일 또는 레거시 시스템과 상호 작용하는 애플리케이션에 있어 필수적인 과정입니다. 프로그래머들은 이를 통해 데이터 교환, 설정 또는 원격 절차 호출을 인간이 읽을 수 있고 기계가 파싱할 수 있는 구조화된, 계층적 형식으로 가능하게 합니다.

## 어떻게:

Dart는 표준 라이브러리에서 XML 처리를 위한 내장 지원을 포함하고 있지 않으므로, 제3자 패키지 사용이 필요합니다. 인기 있는 패키지 중 하나는 `xml`입니다. 사용하기 위해서는 먼저 `pubspec.yaml`에 추가해야 합니다:

```yaml
dependencies:
  xml: ^5.0.0 // 사용 가능한 최신 버전을 사용하세요
```

그런 다음 Dart 파일에 패키지를 import하세요:

```dart
import 'package:xml/xml.dart' as xml;
```

**XML 파싱하기:**

이렇게 XML 문자열이 있다고 가정해 보세요:

```xml
<String name="greeting">안녕하세요, 세상!</String>
```

다음과 같이 XML을 파싱하고 읽을 수 있습니다:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // 출력: greeting
}

void main() {
  final xmlString = '<String name="greeting">안녕하세요, 세상!</String>';
  parseXml(xmlString);
}
```

**XML 문서 생성하기:**

`xml` 패키지를 이용해 새로운 XML 문서를 생성하는 것은 간단합니다:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('안녕하세요, 세상!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**출력**:

```xml
<?xml version="1.0"?>
<greeting name="hello">안녕하세요, 세상!</greeting>
```

**XML 조회 및 수정하기:**

요소를 찾거나 수정하기 위해 XPath와 유사한 메소드를 사용할 수 있습니다:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // 'name' 속성 수정
    greeting.setAttribute('name', 'greeting_modified');
    
    // 새로운 자식 요소 추가
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('안녕!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">안녕하세요, 세상!</greeting>';
  modifyXml(xmlString);
}
```

**출력**:

```xml
<greeting name="greeting_modified">
  안녕하세요, 세상!
  <message>안녕!</message>
</greeting>
```

이 예시들은 Dart에서 XML을 다루기 위한 기본 작업을 보여줍니다. `xml` 패키지를 사용하면, 애플리케이션 요구 사항에 맞게 XML 문서를 파싱하고, 생성하고, 조작할 수 있습니다.
