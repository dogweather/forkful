---
date: 2024-01-26 04:34:00.820735-07:00
description: "\uBC29\uBC95: SimpleXML\uB85C XML \uC77D\uAE30."
lastmod: '2024-04-05T21:53:57.085852-06:00'
model: gpt-4-0125-preview
summary: ''
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 방법:
SimpleXML로 XML 읽기:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Don't forget this</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // 출력: Tove
echo $xml->from;     // 출력: Jani
echo $xml->heading;  // 출력: Reminder
echo $xml->body;     // 출력: Don't forget this
```

DOMDocument로 XML 쓰기:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Don't forget this');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

샘플 출력:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget this</body>
</note>
```

## 심층 분석
XML, 또는 eXtensible Markup Language는 1998년 W3C 추천 이후로 데이터 직렬화에서 핵심 요소가 되어왔습니다. 이는 장황하고, 사람이 읽을 수 있으며, 문법에서 엄격하여 구성 파일, 데이터 교환 등에 신뢰할 수 있는 선택이 되었습니다. 하지만, 웹 API에 대한 그 간결함과 가벼움 때문에 JSON에 일부 가려져 있습니다.

프로그래머들은 종종 XML 스키마에 의해 제공되는 문서 검증이 필요할 때 또는 이미 그것에 크게 의존하는 생태계 내에서 작업할 때(예: Microsoft Office 파일 형식) XML을 선택합니다. PHP에서 XML을 다루는 것은 기본 작업을 위한 SimpleXML 확장을 통해 간단합니다. 더 복잡한 조작을 위해, DOMDocument는 네임스페이스 처리 및 스키마 검증과 같은 더 큰 제어를 허용하는 강력한 기능 세트를 제공합니다.

## 참조
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML 파서](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML 스키마](https://www.w3.org/XML/Schema)
